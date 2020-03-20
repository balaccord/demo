"""
Sends *.JPG from the current folder by GMail using PySimpleGUI

The simpliest files sending ever if your address book is not too big
I personally use it and I'm totally happy with it :) Why didn't I do it earlier?

Code parts from
https://fabianlee.org/2019/10/23/python-sending-html-emails-via-gmail-api-or-smtp-relay/

First of all get the OAuth2 gmail-credentials.json as described here:
https://developers.google.com/identity/protocols/oauth2
and then declare the path to it in the config

The JSON config is created if not exists in the directory where the script is resided
It's name is hardcoded as {scripname}.conf
The contend should be edited manually
The {filename}.pickle contains the authorization data, you should only declare the path to it
Please don't modify it's content
It is not intended for manual modifications
"""

import base64
import copy
import io
import json
import mimetypes
import os
import pickle
import shutil
import socket
import sys
import traceback
from email import utils, encoders
from email.mime import application, multipart, text, base, image, audio
from google.auth.transport.requests import Request
from googleapiclient import discovery, http
from google_auth_oauthlib.flow import InstalledAppFlow
import PySimpleGUI as sg


CONFIG = dict(
    max_Mb=18,
    credentials=f'{os.path.expanduser("~")}{os.sep}gmail-credentials.json',
    token=f'{os.path.expanduser("~")}{os.sep}gmail-token.pickle',
    emails={
        "Joe Doe": "joedoe@dot.com",
        "Judy Doe": "judydoe@dot.com",
    }
)


def get_config_name():
    script = sys.argv[0]
    base = os.path.basename(script)
    name, ext = os.path.splitext(base)
    conf = f"{os.path.dirname(script)}{os.sep}{name}.conf"
    return conf


def read_config():
    global CONFIG
    conf = get_config_name()
    if os.path.exists(conf):
        with open(conf, "r", encoding='utf8') as f:
            CONFIG = json.load(f)
        e = CONFIG['emails']
        CONFIG['emails'] = {k: e[k] for k in sorted(e)}


def write_config():
    global CONFIG

    e = CONFIG['emails']
    CONFIG['emails'] = {k: e[k] for k in sorted(e.keys())}
    conf = get_config_name()
    tmp = f"{conf}.temp"
    with open(tmp, "w", encoding='utf8') as f:
        json.dump(CONFIG, f, indent=2, ensure_ascii=False)
    shutil.move(tmp, conf)


def get_credentials():
    global CONFIG

    token_file = CONFIG['token']
    cred_file = CONFIG['credentials']
    scopes = ['https://www.googleapis.com/auth/gmail.send']

    creds = None
    if os.path.exists(token_file):
        with open(token_file, 'rb') as token:
            creds = pickle.load(token)
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(cred_file, scopes)
            creds = flow.run_local_server(port=0)
        with open(token_file, 'wb') as token:
            pickle.dump(creds, token)
    return creds


def format_size(size_bytes):
    if size_bytes >= 1024 * 1024:
        size_txt = f"{1.0 * size_bytes / 1024 / 1024:.1f} Mb"
    elif size_bytes >= 1024:
        size_txt = f"{1.0 * size_bytes / 1024:.1f} Kb"
    else:
        size_txt = f"{size_bytes} b"
    return size_txt


# https://stackoverflow.com/questions/55542231/how-to-attach-large-files-to-an-email-using-python-gmail-api
# up to 25 Mb
def send_email(email_sender='me', email_to='', email_subject='', email_body_html='', email_cc='',
               email_bcc='', files=None):
    credentials = get_credentials()
    service = discovery.build(serviceName='gmail', version='v1', credentials=credentials)

    message = multipart.MIMEMultipart()
    message['to'] = email_to
    message['from'] = email_sender
    message['date'] = utils.formatdate(localtime=True)
    message['subject'] = email_subject
    message['cc'] = email_cc
    message['bcc'] = email_bcc
    message.attach(text.MIMEText(email_body_html, 'html'))

    for f in files or []:
        f = f.strip(' ')
        mimetype, encoding = mimetypes.guess_type(f)

        if mimetype is None or encoding is not None:
            mimetype = 'application/octet-stream'
        main_type, sub_type = mimetype.split('/', 1)

        if main_type == 'text':
            with open(f, 'rb') as outfile:
                attachement = text.MIMEText(outfile.read(), _subtype=sub_type)
        elif main_type == 'image':
            with open(f, 'rb') as outfile:
                attachement = image.MIMEImage(outfile.read(), _subtype=sub_type)
        elif main_type == 'audio':
            with open(f, 'rb') as outfile:
                attachement = audio.MIMEAudio(outfile.read(), _subtype=sub_type)
        elif main_type == 'application' and sub_type == 'pdf':
            with open(f, 'rb') as outfile:
                attachement = application.MIMEApplication(outfile.read(), _subtype=sub_type)
        else:
            attachement = base.MIMEBase(main_type, sub_type)
            with open(f, 'rb') as outfile:
                attachement.set_payload(outfile.read())

        encoders.encode_base64(attachement)
        attachement.add_header('Content-Disposition', 'attachment', filename=os.path.basename(f))
        message.attach(attachement)

    msg_bytes = message.as_bytes()
    print(f"Message size: {format_size(len(msg_bytes))}")
    media_body = http.MediaIoBaseUpload(io.BytesIO(msg_bytes), mimetype='message/rfc822', resumable=True)
    body_metadata = {}
    print('Sending...')
    try:
        response = service.users().messages().send(userId='me', body=body_metadata, media_body=media_body).execute()
        print(response)
    except errors.HttpError as error:
        print('Error:\n{}'.format(error))


def get_metadata(total, total_size, groups):
    global CONFIG

    input_len = 40

    emails = CONFIG['emails']
    names = list(emails.keys())
    if total_size >= 1024 * 1024:
        size_txt = f"{1.0 * total_size / 1024 / 1024:.1f} Mb"
    elif total_size >= 1024:
        size_txt = f"{1.0 * total_size / 1024:.1f} Kb"
    else:
        size_txt = f"{total_size} b"
    layout = [
        [sg.Text(f"{total} files, {size_txt}, {groups} letters", text_color="black")],
        [sg.Text(f"Subject")],
        [sg.In(key="-subject", default_text="Фото", size=(input_len, 1), font=("Arial", 12), disabled=False)],
        [sg.Text(f"Recipient")],
        [sg.In(key="-email", size=(input_len, 1), font=("Arial", 12))],
        [sg.Text(f"Letter body text")],
        [sg.Multiline(key="-body", default_text='Photo', size=(38, 3), font=("Arial", 12))],
        [sg.Listbox(values=names, size=(input_len - 2, 9), font=("Arial", 12), key="-name", bind_return_key=True,
                    enable_events=True)],
        [sg.Button("Ok", key="ok"), sg.Cancel("Cancel", key="cancel")],
    ]

    # sg.ChangeLookAndFeel('Material1')
    window = sg.Window('Send files', return_keyboard_events=True).Layout(layout)
    while True:  # PySimpleGUI event Loop
        event, values = window.read()
        print({event: values})
        if event == '-files-btn':
            files = values['-files-btn'].split(';')
            if len(files) > 0:
                size = 0.0
                for f in files:
                    size += os.stat(f).st_size
                window['-files-dir'].update(os.path.dirname(files[0]))
                files = [os.path.basename(f) for f in files]
                window['-files'].update(files)
                window['-files-text'].update(f"Total: {len(files)}, {size / 1024 / 1024:.1f} Mb")
        elif event == '-name':
            names = values['-name']
            if len(names) > 0:
                window['-email'].update(emails[names[0]])
        elif event == 'ok':
            if values['-email'] != '':
                break
        elif event in (None, "cancel", "Escape:27"):
            break

    window.close()
    if event == "ok":
        email = values['-email'].strip()
        if not email in emails.values():
            emails[email] = email
            write_config()
        return {
            "email": values['-email'].strip(),
            "subject": values['-subject'].strip(),
            "body": values['-body'].strip(),
        }
    else:
        return None


# -> (filelist, totalsize(bytes))
def get_filelist():
    files = []
    for f in os.listdir('.'):
        if os.path.isfile(f) and os.path.splitext(f)[1].upper() == '.JPG':
            files.append(f)
    files = sorted(files)
    size = 0
    for f in files:
        size += os.stat(f).st_size
    return (files, size)


def split_filelist(filelist, totsize):
    global CONFIG
    max = CONFIG['max_Mb'] * 1024 * 1024

    if totsize <= max:
        return [filelist]

    ret = []
    while len(filelist) > 0:
        size = 0
        for i in range(0, len(filelist)):
            cursize = os.stat(filelist[i]).st_size
            if cursize > max:
                raise Exception(f"Too big {filelist[i]}")
            size += cursize
            if size > max:
                ret.append(filelist[0:i])
                filelist = filelist[i:]
                break
        if len(filelist) > 0 and size < max:
            ret.append(filelist)
            filelist = []
        else:
            size = 0
    return ret


def show_msg(title, msg):
    layout = [
        [sg.Text(title, text_color="black", size=(40, 1), auto_size_text=False)],
        [sg.Text(msg, text_color="black")],
        [sg.Submit("Ok", key="ok")],
    ]
    window = sg.Window(title).Layout(layout)
    while True:  # Event Loop
        event, values = window.read()
        if event == 'ok':
            break
    window.close()


def main():
    read_config()
    old_config = copy.deepcopy(CONFIG)
    files, size = get_filelist()
    groups = split_filelist(files, size)
    msg_count = len(groups)
    meta = get_metadata(len(files), size, msg_count)
    if meta is not None:
        no = 1
        if msg_count > 1:
            subj = f"{meta['subject']} ({no} of {msg_count})"
        else:
            subj = f"{meta['subject']}"
        for g in groups:
            print(subj)
            try:
                send_email(
                    email_sender="me",
                    email_to=meta['email'],
                    email_subject=subj,
                    email_body_html=f"<html><body><br>{meta['body']}<br><br></body></html>",
                    files=g,
                )
            except Exception:
                err_s = traceback.format_exc()
                show_msg("Error", err_s)
                sys.exit(1)
            no += 1
        show_msg("", "Letter(s) sent")


if __name__ == '__main__':
    main()
