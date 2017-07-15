

def rabutanje(energija, rastline):
    n = len(rastline)
    matrika_rezultatov = [[0]* (energija + 1) for i in range(n + 1)]
    for rastlina in range(0, n):
        for ener in range(1, energija + 1):
            seznam_moznosti = []
            for mesto, visina_sadeza in enumerate(rastline[rastlina], start=1):
                if ener - visina_sadeza < 0:
                    seznam_moznosti.append(0)
                else:
                    a = matrika_rezultatov[rastlina][ener - visina_sadeza] + mesto
                    seznam_moznosti.append(a)
            matrika_rezultatov[rastlina + 1][ener] = max(seznam_moznosti)
    return matrika_rezultatov[n][energija]


