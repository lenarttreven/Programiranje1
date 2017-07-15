def kup(n):
    if n in [1,2]:
        return 'B'
    if n in [3,4,5,6,7,8,9,10]:
        return "A"
    n = n - 2
    #zmaga tisti, ki konča na 2, saj potem drugi ne more več odstraniti
    m = n // 13
    #zmaga tisti, ki pride prvi na k * 13
    ostanek = n - m * 13
    if ostanek <= 10 and ostanek >= 3:
        return 'A'
    else:
        return 'B'

#nekdo bo zmagal, in tisti igra na 13-ko enako kot zgoraj
# problem isti, kot da najprej odstranimo prvega, nato drugega,...

def kupi(seznam_kupov):
    if len(seznam_kupov) == 0:
        return None
    if len(seznam_kupov) == 1:
        return kup(seznam_kupov[0])
    if kupi(seznam_kupov[:-1]) == 'A' and kup(seznam_kupov[-1]) == "B":
        return "B"
    elif kupi(seznam_kupov[:-1]) == 'A' and kup(seznam_kupov[-1]) == "A":
        return "B"
    elif kupi(seznam_kupov[:-1]) == 'B' and kup(seznam_kupov[-1]) == "A":
        return "A"
    elif kupi(seznam_kupov[:-1]) == 'B' and kup(seznam_kupov[-1]) == "B":
        return "A"

