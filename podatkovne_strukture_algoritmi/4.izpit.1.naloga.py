


def uspesno_strizenje(seznam, Z, T):
    pretecen_cas = seznam[0]
    for i in seznam:
        if i <= pretecen_cas:
            pretecen_cas += T
        elif i > pretecen_cas:
            pretecen_cas = i + T
    if pretecen_cas <= Z:
        return True
    else:
        return False


# bisekcijo delamo na intervalu 0 in Z


def strizenje(seznam, Z):
    spodaj = 0
    zgoraj = Z
    while zgoraj - spodaj > 10 ** (-6):
        povprecje = (zgoraj + spodaj)/2
        if uspesno_strizenje(seznam, Z , povprecje):
            spodaj = povprecje
        else:
            zgoraj = povprecje
    return round(povprecje, 6)






