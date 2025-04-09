"""

Um exemplo de um problema que pode ser resolvido de forma eficiente com 
programação dinâmica consiste em determinar, dada uma sequência arbitrária de 
números não negativos, se existe uma sub-sequência (não necessariamente contígua) 
cuja soma é um determinado valor. Implemente uma função que dado um valor e uma
listas de listas de números não negativos, devolva a lista com as listas com uma
sub-sequência cuja soma é o valor dado.

"""

# 90%
def adder(soma,lista):
    for i,x in enumerate(lista):
        print("going through",x, "in",lista)
        r = []
        count = 0
        if x == soma:
            return [x]
        elif x < soma:
            count += x
            r.append(x)
        for y in lista[i+1:]:
            print("y:",y,"| r:",r)
            if y+count < soma:
                count += y
                r.append(y)
                res2 = adder(soma-count,lista[i+2:])
                if res2 != []:
                    return r+res2
            elif y+count == soma:
                r.append(y)
                print(r)
                return r
    return []

def validas(soma,listas):
    res = []
    for lista in listas:
        print("Testing:",lista)
        if lista not in res and adder(soma,lista) != []:
            res.append(lista)
         
    #return [l for l in listas if len(l)%2 == 0] easy 50%
    return res