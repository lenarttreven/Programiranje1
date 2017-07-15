import bisect

def poisci_mesto_v_seznamu(seznam, k):
    return bisect.bisect(seznam, k)


def ljudozerci(seznam_ljudozercev, seznam_padalcev):
    for padalec in seznam_padalcev:
        indeks = poisci_mesto_v_seznamu(seznam_ljudozercev, padalec)
        if padalec - seznam_ljudozercev[indeks - 1] <= seznam_ljudozercev[indeks] - padalec:
            seznam_ljudozercev[indeks - 1] = padalec
        else:
            seznam_ljudozercev[indeks] = padalec
    return  seznam_ljudozercev


