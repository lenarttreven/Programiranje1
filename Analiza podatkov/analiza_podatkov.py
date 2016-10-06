import requests
import re
import orodja
import csv
import math
import time



oglasi_na_bolhi = []


file = requests.get('http://www.bolha.com/zivali/male-zivali/macke/?page=1').text
number_of_adverts = re.findall(r'Št. najdenih oglasov: (\d+)', file)
number_of_pages = math.ceil(int(number_of_adverts[0]) / 20)



for stran in range(1, number_of_pages + 1):
    server = 'http://www.bolha.com/zivali/male-zivali/macke/'
    parametri = 'page='
    r = '{}?{}{}'.format(server, parametri, stran)
    orodja.shrani(r, 'bolha/{:02}'.format(stran))
    datoteka = 'bolha/{:02}'.format(stran)
    with open(datoteka) as f:
        vsebina = f.read()
        regex = re.compile(r'div class="ad">.*?<a title="(.+?)".*?</h3>(.+?)<div.*?<div class="price">(?:<span>)?(.*?)(?:</span>)?</div>', re.MULTILINE | re.DOTALL)
        cats = regex.findall(vsebina)
        for cat in cats:
            name, description, price = cat
            name, description, price = name.strip(), description.strip(), price.strip()
            temporary_dic = {'name':name, 'description':description, 'price':price}
            oglasi_na_bolhi.append(temporary_dic)

def write_csv():
    with open('/Users/lenarttreven/Desktop/Programiranje1/Analiza podatkov/vaja.csv', 'w') as f:
        field_names = ['name', 'description', 'price']
        writer = csv.DictWriter(f,fieldnames=field_names)
        for i in oglasi_na_bolhi:
            writer.writerow(i)








