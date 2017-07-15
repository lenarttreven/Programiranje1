
#resitev z rekurizjo
#def pisarji(k, seznam):
#    if len(seznam) == 0:
#        return 0
#    if k == 1:
#        return sum(seznam)
#    if k >= len(seznam):
#        return max(seznam)
#    seznam_rezultatov = []
#    for i in range(len(seznam)):
#        a = sum(seznam[i:])
#        b = pisarji(k - 1, seznam[:i])
#        seznam_rezultatov.append(max(a, b))
#    return min(seznam_rezultatov)


def pisarji(n, knjige):
    k = len(knjige)

    memo = [[float('inf')] + [0 for j in range(n)] for i in range(k + 1)]
    memo[0][0] = 0

    for st_pisarjev in range(1, n+1):
        for st_knjig in range(1,k+1):
            seznam_rezultatov = []
            for s in range(1, st_knjig + 1):
                a = sum(knjige[s-1:st_knjig])
                b = memo[s-1][st_pisarjev-1]
                seznam_rezultatov.append(max(a, b))
            memo[st_knjig][st_pisarjev] = min(seznam_rezultatov)
    return memo[k][n]


#def pisarji(n, knjige):
#    k = len(knjige)
#
#    memo = [[float('inf')] + [0 for j in range(n)] for i in range(k + 1)]
#    memo[0][0] = 0
#
#    for st_pisarjev in range(1, n+1):
#        for st_knjig in range(1,k+1):
#            memo[st_knjig][st_pisarjev] =  min([max(sum(knjige[s-1:st_knjig]), memo[s-1][st_pisarjev-1]) for s in range(1, st_knjig + 1)])
#    return memo[k][n]





pisarji(4, [10, 20, 50, 130, 120, 70, 20])
