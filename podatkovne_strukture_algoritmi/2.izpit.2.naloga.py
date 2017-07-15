from functools import lru_cache


@lru_cache(maxsize=None)
def stevilo_stolpov(n):
    if n == 0:
        return 1
    if n == 1:
        return 1
    if n == 2:
        return 2
    if n == 3:
        return 4
    return stevilo_stolpov(n- 1) + stevilo_stolpov(n - 2) + stevilo_stolpov(n - 3)

@lru_cache(maxsize=None)
def rdeci(n):
    if n == 0:
        return 1
    if n == 1:
        return 1
    if n == 2:
        return 1
    return modri(n - 3) + modri(n - 1)

@lru_cache(maxsize=None)
def modri(n):
    if n == 0:
        return 1
    if n == 1:
        return 1
    return rdeci(n - 2) + rdeci(n - 1)

@lru_cache(maxsize=None)
def barvni_stolp(n):
    return modri(n) + rdeci(n)