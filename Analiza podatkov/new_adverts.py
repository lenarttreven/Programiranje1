import requests
import re
import orodja
import csv
import math
import time


oglasi_na_bolhi = []

def get_site():
    celoten_niz = requests.get('http://www.bolha.com/zivali/male-zivali/macke/?page=1').text

    file = requests.get('http://www.bolha.com/zivali/male-zivali/macke/?page=1').text
    number_of_adverts = re.findall(r'Št. najdenih oglasov: (\d+)', file)
    number_of_pages = math.ceil(int(number_of_adverts[0]) / 20)

    for stran in range(1, number_of_pages + 1):
        server = 'http://www.bolha.com/zivali/male-zivali/macke/'
        parametri = 'page='
        url = '{}?{}{}'.format(server, parametri, stran)
        celoten_niz += requests.get(url).text
    return celoten_niz

def get_dic():
    regex = re.compile(r'div class="ad">.*?<a title="(.+?)".*?</h3>(.+?)<div.*?<div class="price">(?:<span>)?(.*?)(?:</span>)?</div>', re.MULTILINE | re.DOTALL)
    cats = regex.findall(get_site())
    for cat in cats:
        name, description, price = cat
        name, description, price = name.strip(), description.strip(), price.strip()
        temporary_dic = {'name': name, 'description': description, 'price': price}
        oglasi_na_bolhi.append(temporary_dic)


def get_new_ads():
    regex = re.compile(
        r'div class="ad">.*?<a title="(.+?)".*?</h3>(.+?)<div.*?<div class="price">(?:<span>)?(.*?)(?:</span>)?</div>',
        re.MULTILINE | re.DOTALL)
    cats = regex.findall(get_site())
    new_ads = []
    for cat in cats:
        name, description, price = cat
        name, description, price = name.strip(), description.strip(), price.strip()
        advert = {'name': name, 'description': description, 'price': price}
        if advert not in oglasi_na_bolhi:
            new_ads.append(advert)
            oglasi_na_bolhi.append(advert)
    return new_ads

def csv_new_ads():
    if len(get_new_ads()) == 0:
        return 'No new ads'
    else:
        with open('/Users/lenarttreven/Desktop/Programiranje1/Analiza podatkov/new_ads.csv', 'w') as f:
            field_names = ['name', 'description', 'price']
            writer = csv.DictWriter(f, fieldnames=field_names)
            writer.writeheader()
            for i in get_new_ads():
                writer.writerow(i)


import smtplib

from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from os.path import basename
from email.utils import formatdate

def send_email():

    From = 'lenart.test1@gmail.com'
    To = 'lenart.treven44@gmail.com'
    Subject = 'bolha_muce'

    msg = MIMEMultipart()
    msg['From'] = From
    msg['To'] = To
    msg['Date'] = formatdate(localtime=True)
    msg['Subject'] = Subject

    with open('/Users/lenarttreven/Desktop/Programiranje1/Analiza podatkov/new_ads.csv', "rb") as fil:
        part = MIMEApplication(
            fil.read(),
            Name=basename('/Users/lenarttreven/Desktop/Programiranje1/Analiza podatkov/new_ads.csv')
        )
        part['Content-Disposition'] = 'attachment; filename="%s"' % basename('/Users/lenarttreven/Desktop/Programiranje1/Analiza podatkov/new_ads.csv')
        msg.attach(part)

    gmail_user = 'lenart.test1@gmail.com'
    gmail_password = 'programiranje1test1'

    try:
        server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
        server.ehlo()
        server.login(gmail_user, gmail_password)
        server.sendmail(From, To, msg.as_string())
        server.close()

        print('Email sent!')
    except:
        print('Something went wrong...')


def runner():
    while True:
        csv_new_ads()
        send_email()
        time.sleep(10)