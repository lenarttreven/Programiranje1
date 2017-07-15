def fiksna(seznam):
    a = 0
    b = len(seznam) - 1
    najmanjsi = float("inf")
    while a < b:
        c = (a + b) // 2
        if seznam[c] == c + 1:
            najmanjsi = min(c + 1, najmanjsi)
            b = c - 1
        elif seznam[c] > c + 1 :
            b = c - 1
        elif seznam[c] < c + 1:
            a = c + 1
    if seznam[a] == a + 1:
        najmanjsi = min(a + 1, najmanjsi)
    if seznam[b] == b + 1:
        najmanjsi = min(b  + 1, najmanjsi)
    if najmanjsi == float("inf"):
        return None
    else:
        return najmanjsi

