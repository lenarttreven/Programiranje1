"""
Implementacije algoritmov za urejanje
"""


def naivno_uredi(l):
    for i in range(0, len(l) - 1):
        menjava = False
        for j in range(0, len(l) - i - 1):
            if l[j] > l[j + 1]:
                l[j], l[j + 1] = l[j + 1], l[j]  # swap
            menjava = True
        if menjava == False:
            break
    return l


def vgrajeni_sort(l):
    l.sort()


def naredi_seznam(sez):
    manjsi = []
    vecji = []
    pivot = sez[0]
    for i in sez[1:]:
        if i <= pivot:
            manjsi.append(i)
        else:
            vecji.append(i)
    return (pivot, manjsi, vecji)

def hitro_uredi_z_novimi_seznami(l):
    if len(l) == 0:
        return l
    else:
        pivot, manjsi, vecji = naredi_seznam(l)
        return hitro_uredi_z_novimi_seznami(manjsi) + [pivot] + hitro_uredi_z_novimi_seznami(vecji)

def zamenjaj(seznam, i, j):
    seznam[i], seznam[j] = seznam[j], seznam[i]


def delitev_na_mestu(seznam, zacetek, konec):
    pivot = seznam[konec - 1]
    pozicija = zacetek - 1
    mesto_dodajanja = 2
    for _ in range(zacetek - 1, konec - 1):
        if seznam[pozicija] > pivot:
            zamenjaj(seznam, pozicija, konec - mesto_dodajanja)
            mesto_dodajanja += 1
        else:
            pozicija += 1
    zamenjaj(seznam, konec - 1, pozicija)
    return pozicija + 1


def hitro_uredi_na_mestu(seznam, zacetek=1, konec=None):
    if konec is None:
        konec = len(seznam)
    if konec - zacetek < 1:
        return
    else:
        pivot = delitev_na_mestu(seznam, zacetek, konec)
        hitro_uredi_na_mestu(seznam, zacetek, pivot - 1)
        hitro_uredi_na_mestu(seznam, pivot + 1, konec)


def zlij(levi, desni):
    skupni_seznam = []
    indeks_levega = 0
    indeks_desnega = 0
    for i in range(len(levi)+len(desni)):
        if indeks_desnega == len(desni):
            skupni_seznam += levi[indeks_levega:]
            break
        elif indeks_levega == len(levi):
            skupni_seznam += desni[indeks_desnega:]
            break
        elif levi[indeks_levega] <= desni[indeks_desnega]:
            skupni_seznam.append(levi[indeks_levega])
            indeks_levega += 1
        else:
            skupni_seznam.append(desni[indeks_desnega])
            indeks_desnega += 1
    return skupni_seznam



def razdeli(seznam):
    if len(seznam) == 1:
        return seznam
    else:
        return (seznam[:len(seznam)//2], seznam[len(seznam)//2 :]) #


def uredi_z_zlivanjem(seznam):
    if len(seznam) <= 1:
        return seznam
    else:
        levi, desni = razdeli(seznam)
    return zlij(uredi_z_zlivanjem(levi), uredi_z_zlivanjem(desni))
