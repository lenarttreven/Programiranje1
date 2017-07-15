def kti_najmanjsi(l, k, a=None, b=None):
    if a is None:
        a = 0
    if b is None:
        b = len(l) - 1
    pivot = l[a]
    za = a + 1
    ko = b
    i = a + 1
    while i <= b:
        if i > ko:
            break
        if l[i] <= pivot:
            l[za], l[i] = l[i], l[za]
            za += 1
            i += 1
        else:
            l[ko], l[i] = l[i], l[ko]
            ko -= 1
    l[a], l[za - 1] = l[za - 1], l[a]
    # Pivot je na mestu za - 1
    if za - a == k:
        return za - 1
    elif za - a < k:
        return kti_najmanjsi(l, k - (za - a), za, b)
    else:
        return kti_najmanjsi(l, k, a, za - 2)

def po_velikosti(seznam, k, m):
    a = kti_najmanjsi(seznam, k)
    nov_seznam = seznam[a:]
    b = kti_najmanjsi(nov_seznam, m + 1)
    nov_seznam1 = nov_seznam[:b]
    nov_seznam1.sort()
    return nov_seznam1

po_velikosti([2, 7, 1, 9, 2, 3, 2, 5], 3, 4)