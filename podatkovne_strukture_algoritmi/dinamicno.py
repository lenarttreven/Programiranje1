# Miška
# =====
#
# Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
# samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
# desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
# različne mase. Miška bi se rada kar se da nažrla, zato jo zanima, katero pot
# naj ubere. Napišite funkcijo max_sircka(matrika_sirckov), ko dobi matriko z
# masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
# optimalni poti.
#
#
# predstavitev




 # naivno
 #def max_sircka(matrika_sircka):
 #    kolicina_sira = 0
 #    kolicina_sira += matrika_sircka[0][0]
 #    desno = max_sircka(matrika_sircka[:][1:])
 #    dol = max_sircka(matrika_sircka[1:][:])
 #    kolicina_sira += max(desno dol)
 #    return kolicina_sira #


def max_sirckov(matrika_sirckov):
    vrstice = len(matrika_sirckov)
    stolpci = len(matrika_sirckov[0])
    matrika_rezultatov = [[0 for _ in range(stolpci)] for _ in range(vrstice)]
    for vr in range(vrstice - 1, -1, -1):
        for st in range(stolpci -1, -1, -1):
            if vr == vrstice - 1 and st == stolpci - 1:
                matrika_rezultatov[vr][st] = matrika_sirckov[vr][st]
            elif vr == vrstice - 1:
                matrika_rezultatov[vr][st] += matrika_sirckov[vr][st] + matrika_rezultatov[vr][st + 1]
            elif st == stolpci - 1:
                matrika_rezultatov[vr][st] += matrika_sirckov[vr][st] + matrika_rezultatov[vr + 1][st]
            else:
                matrika_rezultatov[vr][st] += matrika_sirckov[vr][st] + max(matrika_rezultatov[vr + 1][st], matrika_rezultatov[vr][st + 1])
    return matrika_rezultatov[0][0]


max_sirckov([[1,2,3],[4,5,6],[7,8,9]])

# Nahrbtnik
# =========
#
# Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
# poljubne izdelke, katerih skupna masa ne presega k kilogramov. (Podelili so
# tri nagrade in sicer s parametrom k = 1, k = 2 in k = 5). Napišite funkcijo
# nahrbtnik(seznam_artiklov, k), ki poišče največjo ceno, ki jo lahko odnesemo
# iz trgovine. Naredite dve verziji: pri prvi lahko vzamemo samo en artikel iste
# vrste, pri drugi pa poljubno število artiklov iste vrste.

izdelki = [
	('jogurt', 0.39, 0.18),
	('mleko', 0.89, 1.03),
    ('kava', 2.19, 0.2),
    ('maslo', 1.49, 0.25),
    ('kvas', 0.22, 0.042),
    ('jajca', 2.39, 0.69),
    ('klobasa', 3.76, 0.50),
    ('čebula', 1.29, 2.0),
    ('kruh', 2.99, 1.0),
    ('Nutella', 4.99, 0.75),
    ('sok', 1.15, 2.0)
]


def memo(f):
    izracunane = {}

    def memo_f(*args):
        if args not in izracunane:
            izracunane[args] = f(*args)
        return izracunane[args]

    return memo_f


def nahrbtnik_01(seznam_artiklov, k):
    if seznam_artiklov == []:
        return 0
    if k == 0:
        return 0
    ime, cena, teza = seznam_artiklov[0]
    if teza <= k:
        cenaZ = cena + nahrbtnik_01(seznam_artiklov[1:], k - teza)
    else:
        cenaZ = 0
    cenaBrez = nahrbtnik_01(seznam_artiklov[1:], k)
    return max(cenaZ, cenaBrez)

print(nahrbtnik_01(izdelki, 5))


def poljubno_artiklov(izdelki):
    pass

# Jajca
# =====
#
# Živimo v visoki stolpnici, ki ima n nadstropij. Imamo škatlo k jajc, ki so menda zelo trpežna,
# saj naj bi prenesla padce z višjih nadstropij stoplnice. Radi bi ugotovili, katero je najvišje
# nadstopje, pri katerem jajca še preživijo padec. Ker nimamo veliko časa, bi radi poiskali
# strategijo, pri kateri bomo minimizirali število metov.
#
# Razmislite:
#  * Kako moramo ravnati v primeru, ko imamo samo eno jajce?
#  * Kako lahko ravnamo v primeru, ko imamo na voljo zelo veliko jajc (več kot je število
#    nadstropij)?
#
# Napišite funkcij, ki bo izračunala maksimalno število metov (v najslabšem primeru), da ugotovimo
# številko kritičnega nadstropja, če imamo na voljo točko k jajc.

from functools import lru_cache


#@memo

@lru_cache(maxsize=None)
def max_stevilo_metov(n, k): #(nadstropja, jajca)
    if n == 0:
        return 0
    if n == 1 and k >= 1:
        return 1
    if k == 1:
        return n
    stevila = []
    for met in range(n): #vrzemo iz met -tega nadstropja
        stevilo_ce_se_razbije = 1 + max_stevilo_metov(met, k - 1)
        stevilo_ce_se_ne_razbije = 1 + max_stevilo_metov(n - met - 1, k)
        stevila.append(max(stevilo_ce_se_ne_razbije, stevilo_ce_se_razbije))

    return min(stevila)


print(max_stevilo_metov(100,2))