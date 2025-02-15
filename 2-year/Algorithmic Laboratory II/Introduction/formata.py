"""

Implemente uma função que formata um programa em C.
O código será fornecido numa única linha e deverá introduzir
um '\n' após cada ';', '{', ou '}' (com excepção da última linha).
No caso do '{' as instruções seguintes deverão também estar identadas
2 espaços para a direita.

"""

# 100%
def formata(codigo):
    res = []
    ind = 0
    i = 0
    wsc = 0
    flag = False
    flag2 = False # true if [-1] c != ws
    #print(codigo)
    for c in codigo[:-1]:
        if c == ';':
            flag = True
            flag2 = False
        if c == '}':
            flag = True
            flag2 = False
            ind -= 2
            res.append(' '*ind)
        elif c == '{':
            flag = True
            flag2 = False
            ind += 2
        if c == ' ':
            wsc += 1
            if wsc == 1 and flag2 :
                res.append(c)
                flag2 = False
        else:
            flag2 = True
            wsc = 0
            res.append(c) 
        if flag:
            res.append('\n')
            #if i != len(codigo)-2:
            if codigo[i+1] != '}':
                res.append(' '*ind)
            flag = False
            flag2 = False
        i += 1
    if codigo[-1] != ' ':
        res.append(codigo[-1])
    return "".join(res)