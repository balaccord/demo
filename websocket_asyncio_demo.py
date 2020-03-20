"""
Python coding skills example
Demo of the real websocket live stream logger, only class names and comments are changed
The Google Chrome Dev Protocol is used for the live data capture
"""
from PyQt5.QtWidgets import QMainWindow
from PyQt5.QtCore import pyqtSlot
import asyncio
import pprint
import logging
import logging.handlers
import logging.config
import ujson as json  # ultra fast JSON
import os
import copy
import pandas as pd
import warnings
import time
import aiochrome
import timer_cm
from PyQt5.QtWidgets import QApplication
from asyncqt import QEventLoop
import sys
import attr
import csv

# http://threeofwands.com/attrs-i-the-basics/
# noinspection PyProtectedMember
from attr import NOTHING
# noinspection PyProtectedMember
from attr._make import _CountingAttr

from demo_site_main import Ui_MainWindow


def add_defaults(cl):
    for obj in cl.__dict__.values():
        # noinspection PyProtectedMember
        if not isinstance(obj, _CountingAttr) or obj._default is not NOTHING:
            continue
        obj._default = None
    return cl

# PyCharm warnings suppression
# https://www.jetbrains.com/help/pycharm/suppressing-inspections.html


class LogTimer(timer_cm.Timer):
    def __init__(self, name, print_results=True, log=None, level=logging.INFO):
        super().__init__(f'LogTimer: {name}', print_results)
        self._log = log
        self._level = level

    def print_results(self):
        log = logging if self._log is None else self._log
        log.log(self._level, self._format_results())


def timeit(coro):
    async def helper(*args, **params):
        name = coro.__name__
        sync = not asyncio.iscoroutinefunction(coro)
        logging.debug(f'timeit start: {name}')
        start = time.time()
        if sync:
            result = coro(*args, **params)
        else:
            result = await coro(*args, **params)
        elapsed = time.time() - start
        long = ' (LONG!!!)' if elapsed >= 0.03 else ''
        logging.debug(f'timeit stop{long}: {elapsed:.5f} {name}')
        return result

    return helper


# https://stackoverflow.com/questions/33128325/how-to-set-class-attribute-with-await-in-init
# @classmethod
async def new_browsertab(port, aioloop):

    @attr.s(slots=True)
    @add_defaults
    class BrowserTab(object):
        log = attr.ib()
        log_demo_site_ohlc = attr.ib()
        log_demo_site_sentiment = attr.ib()
        log_demo_site_winperc = attr.ib()
        log_demo_site_margin = attr.ib()
        symbol = attr.ib()
        symbols = attr.ib()
        candles = attr.ib()
        quotes = attr.ib()
        account_sum = attr.ib()
        count = attr.ib()
        tf = attr.ib()
        skip_log = attr.ib()
        browser = attr.ib()

        _tab = attr.ib()
        _port = attr.ib()
        _aioloop = attr.ib()

        # @timeit
        async def async_get_noticeable_data(self, caller_name, **kwargs):
            try:
                s = kwargs["response"]["payloadData"]
                j = json.loads(s)
                if not isinstance(j, list):
                    self.log.warning(f"{caller_name}: Scalar instead of list '{j}'")
                    j = [j]
                return j
            except Exception:
                self.log.exception(f'{caller_name}: Decode error: {kwargs}')
                raise

        async def async_websocket_frame_received(self, **kwargs):
            if self.log.isEnabledFor(logging.DEBUG):
                logging.debug(f"websocket_frame_received: {kwargs}")
            dispatch = {
                **{i: None for i in [3, 90]},

                52: self.async_got_52_account_sum,
                70: self.async_got_70_symbol_list,

                1: self.async_got_1_quote,
                2: self.async_got_2_ohlc,
                4: self.async_got_4_quote_ohlc,
                72: self.async_got_72_winperc_list,
                73: self.async_got_73_sentiment_list,
                80: self.async_got_80_got_margins,
                95: self.async_got_95_ack_symbol,
            }
            log_info = {  # forced standard data logging for items in list
                i: 1 for i in [None]
            }
            log_debug = {  # forced debug data logging for items in list
                i: 1 for i in [None]
            }
            j = await self.async_get_noticeable_data("websocket_frame_received", **kwargs)
            for row in j:
                e = int(row["e"])
                del row["e"]
                if e in dispatch:
                    coro = dispatch[e]
                    if coro is not None:
                        await coro(e, row)
                if e in log_info:
                    self.log.info(f"{e} (DEBUG): {row}")
                if e not in dispatch or e in log_debug:
                    eventloop.create_task(self.async_debug_frame(e, row))

        # @timeit
        async def async_websocket_frame_sent(self, **kwargs):
            j = await self.async_get_noticeable_data("websocket_frame_sent", **kwargs)

            self.log.info("websocket_frame_sent: %s", j)

        # @timeit
        async def async_websocket_created(self, **kwargs):
            self.log.debug("async_websocket_created AS IS: %s", kwargs)

        # @timeit
        async def async_websocket_will_send_handshake_request(self, **kwargs):
            self.log.debug("websocket_will_send_handshake_request: %s", kwargs)

        # @timeit
        async def async_websocket_handshake_response_received(self, **kwargs):
            self.log.info("websocket_handshake_response_received: %s", kwargs)

        # @timeit
        async def async_websocket_closed(self, **kwargs):
            self.log.debug("websocket_closed AS IS: %s", kwargs)

        async def async_go(self, url="https://demo_site.com/platform", timeout=5):
            await self._tab.call_method("Page.navigate", url=url, _timeout=timeout)

        # shortcut for the convenience
        # @timeit
        async def async_debug_frame(self, e, j, keep_intact=True):
            """

                  :param e: opcode
                  :param j: data
                  :param keep_intact: process the copy, not original data (slows execution)
                  :return:
                  """

            if e in self.skip_log:
                return
            if keep_intact:
                j = copy.deepcopy(j)
            if e in (3, 4):
                for d in j['d']:
                    if 'quotes' in d:
                        d['quotes'] = d['quotes'][:2]
                    if 'candles' in d:
                        d['candles'] = d['candles'][:2]
            elif e in (70, 73):
                j['d'] = j['d'][:2]
            elif e in (80,):
                for d in j['d']:
                    d['d'] = d['d'][:2]
            # simplify a little
            if e in (1, 2, 52, 72, 111):  # 70. 73
                j = j['d'][0]
            # logging.debug(f"{e}: {j}")
            self.log.debug(f"{e}: %s", j)

        # @timeit
        async def async_got_1_quote(self, e, j):
            try:
                jj = j["d"][0]
                t = jj['t']
                # assert name == current_pair, f'Quote for {name} instead of {current_pair} : {j}'
                if self.quotes is None:  # if not hasattr(self, 'quotes'):
                    # noinspection PyAttributeOutsideInit
                    self.quotes = pd.DataFrame(jj, columns=['q'], index=[t])
                else:
                    self.quotes.at[t] = {'q': jj['q']}
                if e not in self.skip_log:
                    self.log.debug(f"{e}: Quote: {jj['j']}")
                    # self.log.debug(f"Quote data size: {len(self.quotes.index)}")
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        # @timeit
        async def async_got_2_ohlc(self, e, j):
            cols = ['open', 'high', 'low', 'close']
            try:
                jj = j["d"][0]
                t = jj['t']
                if self.candles is None:  # if not hasattr(self, 'candles'):
                    # noinspection PyAttributeOutsideInit
                    self.candles = pd.DataFrame(jj, columns=cols, index=[t])
                else:
                    self.log_demo_site_ohlc.info(
                        f"{jj['t']},{jj['p']},{jj['open']},{jj['high']},{jj['low']},{jj['close']}")
                if e not in self.skip_log:
                    self.log.debug(f"{e}: Candle: {{k: jj[k] for k in cols}}")
                    # self.log.debug(f"Candles size: {len(self.candles.index)}")
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        @timeit
        async def async_got_4_quote_ohlc(self, e, j):
            skip_log = e in self.skip_log
            try:
                if j["d"] is None:
                    self.log.debug(f"{e}: No data")
                    return
                alen = len(j["d"])
                assert alen == 1, f"{e}: array length is more than 1"

                uuid = f', uuid: {j["uuid"]}' if "uuid" in j else ''
                jj = j["d"][0]

                # current_pair = self.symbol["pair"]
                # assert name == current_pair, f'The data are not for the current pair ' \
                #     f'{current_pair}:{self.symbol["uuid"]} - {j}'

                # noinspection PyAttributeOutsideInit
                self.tf = jj['tf']
                # noinspection PyAttributeOutsideInit
                self.symbol = jj["p"]
                # self.log_demo_site_ohlc.info(f"; [{self.symbol}]")
                sub_uid = f', sub_uid: {jj["sub_uid"]}' if "sub_uid" in jj else ''
                if not skip_log:
                    self.log.info(f"{e}: Pair: {self.symbol}, timeframe: {self.tf}{uuid}{sub_uid}")

                with LogTimer('PD.CANDLES', level=logging.DEBUG):
                    if "candles" in jj:  # 6 ms
                        df = pd.DataFrame(jj["candles"])
                        df.set_index('t', inplace=True)
                        df_csv = df.copy()
                        df_csv.sort_index(inplace=True)
                        df_csv["symbol"] = self.symbol
                        self.log_demo_site_ohlc.info(f"; BATCH BEGIN")
                        self.log_demo_site_ohlc.info(
                            df_csv.to_csv(
                                header=False,
                                columns=['symbol', 'open', 'high', 'low', 'close'],
                                line_terminator="\n",
                                quoting=csv.QUOTE_NONE,
                            ))
                        self.log_demo_site_ohlc.info(f"; BATCH END")
                        if self.candles is None:
                            self.candles = df
                        else:
                            self.candles = df
                        if not skip_log:
                            self.log.debug(f"{e}: Candles: {len(df.index)}")
                        await asyncio.sleep(0)
                    else:
                        # noinspection PyAttributeOutsideInit
                        self.candles = None

                with LogTimer('PD.QUOTES', level=logging.DEBUG):
                    if "quotes" in jj:  # 8 ms
                        df = pd.DataFrame(jj["quotes"])
                        df.set_index(['t'], inplace=True)
                        if self.quotes is None:
                            self.quotes = df
                        else:
                            self.quotes = pd.concat([self.quotes, df]).drop_duplicates()
                        if not skip_log:
                            self.log.debug(f"{e}: Quotes: {len(df.index)}")
                    else:
                        # noinspection PyAttributeOutsideInit
                        self.quotes = None
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        # @timeit
        async def async_got_52_account_sum(self, e, j):
            try:
                val = j["d"][0]["value"]

                # noinspection PyAttributeOutsideInit
                self.account_sum = val
                if e not in self.skip_log:
                    self.log.debug(f"{e}: Account remainder: {val}")
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        # @timeit
        async def async_got_70_symbol_list(self, e, j):
            try:
                df = pd.DataFrame(j["d"])  # 90 ms
                await asyncio.sleep(0)
                df.set_index('name', inplace=True)

                # noinspection PyAttributeOutsideInit
                self.symbols = df
                if e not in self.skip_log:
                    keys = sorted(df.index.values.tolist())
                    self.log.debug(f"{e}: Pairs received: {keys!s}")
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        # @timeit
        async def async_got_72_winperc_list(self, e, j):
            if self.symbols is None:
                self.log.warning(f"{e}: Margin got before the pairs list: {j}")
                return
            try:
                t = int(time.time())
                jd = j["d"]
                self.log_demo_site_winperc.info(
                    "\n".join(((f"{t},{row['pair']},{row['winperc']}" for row in jd)))
                )
                for jj in jd:
                    self.symbols.at[jj["pair"], "winperc"] = jj['winperc']
                if e not in self.skip_log:
                    self.log.info(f"{e}: Margin: {j['d']}")
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        @timeit
        async def async_got_73_sentiment_list(self, e, j):
            if self.symbols is None:
                self.log.warning(f"{e}: Sentiments got before the pairs list: {j}")
                return
            try:
                t = int(time.time())
                jd = j["d"]
                self.log_demo_site_sentiment.info(
                    "\n".join(((f"{t},{row['pair']},{row['sentiment']}" for row in jd)))
                )
                existing_keys_dict = {k: 1 for k in self.symbols.index.values.tolist()}

                existing_keys_data_list = [row for row in jd if row['pair'] in existing_keys_dict]
                keys = [row['pair'] for row in existing_keys_data_list]
                values = [[row['sentiment']] for row in existing_keys_data_list]
                self.symbols.loc[keys, 'sentiment'] = values

                non_existing_keys_data_list = [row for row in jd if row['pair'] not in existing_keys_dict]
                symbols_dict = {k: None for k in list(self.symbols.columns)}
                for k in non_existing_keys_data_list:
                    symbols_dict["sentiment"] = k['sentiment']
                    self.symbols.at[k["pair"]] = symbols_dict
                    pass
                if e not in self.skip_log:
                    self.log.info(f"{e}: Sentiments: {j['d']}")
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        def async_got_80_got_margins(self, e, j):
            try:
                self.log_demo_site_margin.info(j)
            except Exception:
                self.log.exception(f"{e}: Wrong data format: {j}")
                raise

        async def async_got_95_ack_symbol(self, e, j):
            try:
                self.symbol = j["d"][0]["pair"]
            except Exception:
                self.log.exception(f"Wrong data format (e): {j}")
                raise

    browsertab = BrowserTab(
        port=port,
        aioloop=aioloop,
        skip_log={i: 1 for i in [1, 2, 72, 73]},
        log=logging.getLogger('BrowserTab'),
        log_demo_site_ohlc=logging.getLogger("demo_site_ohlc"),
        log_demo_site_sentiment=logging.getLogger("demo_site_sentiment"),
        log_demo_site_winperc=logging.getLogger("demo_site_winperc"),
        log_demo_site_margin=logging.getLogger("demo_site_margin"),
        browser=aiochrome.Browser(url=f"http://127.0.0.1:{port!s}", loop=aioloop)
    )
    tab = await browsertab.browser.new_tab()  # if newtab else await browsertab._browser.list_tab()[0]
    browsertab._tab = tab

    tab.set_listener("Network.webSocketFrameSent", browsertab.async_websocket_frame_sent)
    tab.set_listener("Network.webSocketFrameReceived", browsertab.async_websocket_frame_received)

    await tab.start()
    await tab.call_method("Network.enable")
    # handlers
    # https://chromedevtools.github.io/devtools-protocol/tot/Network#event-webSocketClosed
    # chrome://net-internals/#events&q=type:SOCKET%20is:active
    await browsertab.async_go()
    pass


# Quick and dirty JSON logger
# Example: log.debug("Info: ", info)
# not a speed demon because of deepcopy and pprint at once
class JsonFormatter(logging.Formatter):
    def format(self, record):
        if record.args:
            newrec = copy.deepcopy(record)
            newrec.args = pprint.pformat(newrec.args, compact=True)
            return super().format(newrec)
        else:
            return super().format(record)


class DemoSiteOHLCTimedRotatingFileHandler(logging.handlers.TimedRotatingFileHandler):
    def doRollover(self):
        super().doRollover()
        logging.getLogger("demo_site_ohlc").info("; TIME, SYMBOL, OPEN, HIGH, LOW, CLOSE")


def setup_logging():
    # How to Create Rotating Logs
    # http://www.blog.pythonlibrary.org/2014/02/11/python-how-to-create-rotating-logs/

    # optimization
    # https://docs.python.org/3.7/howto/logging.html#optimization

    # configuration
    # http://www.blog.pythonlibrary.org/2012/08/02/python-101-an-intro-to-logging/
    # https://docs.python.org/3/howto/logging.html#configuring-logging

    # https://docs.python.org/3/library/logging.config.html#configuration-dictionary-schema
    logging.config.dictConfig({
        "version": 1,
        "formatters": {
            "default": {
                # https://docs.python.org/3.6/library/logging.html#logrecord-attributes
                "format": '%(asctime)-15s %(threadName)-10s %(levelname)-7s %(name)s: %(message)s'
            },
            "plain": {
                "format": '%(message)s'
            },
            "json": {
                # it was found there's a "genuine" module there
                # https://github.com/madzak/python-json-logger
                "()": "__main__.JsonFormatter",
                "format": '%(asctime)s\n%(message)s\n'
            }
        },
        # "filters": {},
        "handlers": {
            "file": {
                "class": "logging.handlers.RotatingFileHandler",
                "level": "DEBUG",
                "formatter": "default",
                # "filters": [],
                "filename": f'logs\\{basename}.log',
                "maxBytes": 1024 * 1024 * 10,
                "backupCount": 5
            },
            "console": {
                "class": "logging.StreamHandler",
                "formatter": "default",
                "level": "INFO",
                # "filters": [allow_foo]
                "stream": "ext://sys.stdout"
            },
            "websocket_debug": {
                "class": "logging.handlers.RotatingFileHandler",
                "level": "NOTSET",
                "formatter": "json",
                # "filters": [],
                "filename": f'logs\\{basename}_ws.log',
                "maxBytes": 1024 * 1024 * 10,
                "backupCount": 5
            },
            "demo_site_ohlc": {
                # "class": "logging.handlers.TimedRotatingFileHandler",
                "class": "__main__.DemoSiteOHLCTimedRotatingFileHandler",
                "level": "NOTSET",
                "formatter": "plain",
                # "filters": [],
                "filename": r'logs\demo_site_ohlc.log',
                "when": "h",
                "interval": 1,
                "backupCount": 0
            },
            "demo_site_sentiment": {
                "class": "logging.handlers.TimedRotatingFileHandler",
                "level": "NOTSET",
                "formatter": "plain",
                # "filters": [],
                "filename": r'logs\demo_site_sentiment.log',
                "when": "h",
                "interval": 1,
                "backupCount": 0
            },
            "demo_site_winperc": {
                "class": "logging.handlers.TimedRotatingFileHandler",
                "level": "NOTSET",
                "formatter": "plain",
                # "filters": [],
                "filename": r'logs\demo_site_winperc.log',
                "when": "h",
                "interval": 1,
                "backupCount": 0
            },
            "demo_site_margin": {
                "class": "logging.handlers.TimedRotatingFileHandler",
                "level": "NOTSET",
                "formatter": "plain",
                # "filters": [],
                "filename": r'logs\demo_site_margin.log',
                "when": "h",
                "interval": 1,
                "backupCount": 0
            },
        },
        "root": {
            "handlers": ["file", "console"],
            "level": "DEBUG",
        },
        "loggers": {
            "demo_site_ohlc": {
                "handlers": ["demo_site_ohlc"],
                "level": "NOTSET",
                "propagate": False,
            },
            "demo_site_sentiment": {
                "handlers": ["demo_site_sentiment"],
                "level": "NOTSET",
                "propagate": False,
            },
            "demo_site_winperc": {
                "handlers": ["demo_site_winperc"],
                "level": "NOTSET",
                "propagate": False,
            },
            "demo_site_margin": {
                "handlers": ["demo_site_margin"],
                "level": "NOTSET",
                "propagate": False,
            },
            # websocket debuging log used in the appropriate class
            "BrowserTab": {
                "handlers": ["websocket_debug"],
                "level": "NOTSET",
            },
            # rewrite value for the third-party module
            # "aiochrome.tab": {
            #     "level": "DEBUG",
            # },
        },
    })
    logging.info("====================================================================================")
    logging.info("\t\tSTART")
    logging.info("====================================================================================")
    logging.getLogger("demo_site_ohlc").info("; TIME, SYMBOL, OPEN, HIGH, LOW, CLOSE")


class MainWindow(QMainWindow, Ui_MainWindow):

    def __init__(self, parent=None):
        QMainWindow.__init__(self, parent=parent)
        self.setupUi(self)

    @pyqtSlot()
    def tbl_changed(self):
        pass

    @pyqtSlot(int)
    def pair_changed(self, i=0):
        pass

    @pyqtSlot()
    def app_quit(self):
        global app
        # self.hide()
        app.quit()


if __name__ == "__main__":
    # asyncio debugging
    # https://docs.python.org/3.6/library/asyncio-dev.html
    # https://docs.python.org/3/using/cmdline.html#environment-variables
    os.environ['PYTHONASYNCIODEBUG'] = "1"
    os.environ['DEBUG'] = "1"
    warnings.warn("always", ResourceWarning)

    basename = os.path.splitext(os.path.basename(__file__))[0]

    # global app
    app = QApplication(sys.argv)
    eventloop = QEventLoop(app)
    asyncio.set_event_loop(eventloop)
    eventloop.set_debug(True)
    # executor = concurrent.futures.ThreadPoolExecutor(5)
    # eventloop.set_default_executor(executor)

    setup_logging()

    window = MainWindow()
    window.show()

    with eventloop:
        eventloop.create_task(new_browsertab(9232, eventloop))
        sys.exit(eventloop.run_forever())
        pass
    pass
