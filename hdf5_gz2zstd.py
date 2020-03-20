import gzip
import lzma
import numpy as np
import pandas as pd
import zstandard as zstd
import time
import os

"""
HDF5 storage GZ to ZSTD (custom dict) repack utility
"""

# os.environ['PYTHON_ZSTANDARD_IMPORT_POLICY'] = "cffi"
import h5py


class Hdf5HtmlIter(object):

    def __init__(self, ds, max=None):
        self._ds = ds
        self._cur = 0
        self._max = max

    def __iter__(self):
        return self

    def __next__(self):
        if self._max is not None:
            if self._cur >= self._max:
                raise StopIteration
        try:
            gz = self._ds[self._cur]
        except ValueError:
            raise StopIteration
        self._cur += 1
        return gzip.decompress(gz.tobytes())


def ds_append(ds, data, expand_by=100):
    try:
        recno = ds.attrs["lastrec"] + 1
    except KeyError:
        recno = 0

    dslen = ds.len()
    #     print(f"recno {recno} ds.len() {dslen}")
    if recno >= dslen:
        ds.resize([dslen + expand_by])
    ds[recno] = data
    ds.attrs["lastrec"] = recno


def main():
    zdict = r"custom_zstd_dict"
    with open(zdict, "rb") as f:
        zdict = f.read()
    zdict = zstd.ZstdCompressionDict(zdict)
    zc = zstd.ZstdCompressor(level=12, dict_data=zdict,
                             threads=6,
                             # write_content_size=False,
                             write_checksum=True,
                             write_dict_id=False)
    zd = zstd.ZstdDecompressor(dict_data=zdict)

    def process(write=True, max=None):
        stats = []
        with h5py.File(r"html_gz.h5", "r") as rf:
            with h5py.File(r"html_zstd.h5", "w") as wf:
                rds = rf["/html_gz"]
                # sort of varbinary(), somewhat tricky but usable
                wds = wf.create_dataset('html_zstd', (0,), maxshape=(None,), dtype=h5py.special_dtype(vlen=np.uint8))
                t0 = time.perf_counter()
                records = 0
                for gz in rds:
                    records += 1
                    t1 = time.perf_counter()
                    html = gzip.decompress(gz.tobytes())
                    zs = zc.compress(html)
                    zs = np.frombuffer(zs, dtype=np.uint8)
                    hlen = len(html)
                    glen = len(gz)
                    zlen = len(zs)
                    t2 = time.perf_counter()
                    stats.append([hlen / 1024, hlen / glen, hlen / zlen, (t2 - t1) * 1000])
                    if write:
                        ds_append(wds, zs)
                    if records % 250 == 0:
                        df = pd.DataFrame(columns=["html", "gz", "zstd", "ms"], data=stats)
                        df["gz/zstd"] = df["zstd"] / df["gz"]
                        df = df.agg("mean")
                        dt = time.perf_counter() - t0
                        rec_per_sec = records / dt
                        remain = rds.len() - records
                        print(f"html: {df.loc['html']:.1f}k,",
                              ", ".join([f"{k:}: {df.loc[k]:>.2f}" for k in ("gz", "zstd", "gz/zstd")]))
                        print(
                            f"{records} records processed in {dt / 60:.1f} min, {rec_per_sec:.0f} rec/s, {remain} remaining, ETA {remain / rec_per_sec / 60:.1f} min")
                        stats = []
                    if max is not None:
                        if records > max:
                            break

    process()  # max=5000)


if __name__ == '__main__':
    main()
