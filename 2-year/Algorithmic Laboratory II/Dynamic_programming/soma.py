"""

Implemente uma função que calula qual a subsequência (contígua e não vazia) de 
uma sequência de inteiros (também não vazia) com a maior soma. A função deve 
devolver apenas o valor dessa maior soma.

Sugere-se que começe por implementar (usando recursividade) uma função que 
calcula o prefixo de uma sequência com a maior soma. Tendo essa função 
implementada, é relativamente adaptá-la para devolver também a maior soma de toda
a lista.

"""

# 60%
def recursiva(lista,r): #Alterar recursiva para verificar o maximo antes de recursivar
    #print(lista)
    if lista == []: 
        r.append((0,[]))
        return r
    if len(lista) == 1:
        if len(r) > 0:
            r.append((lista[0] + r[-1][0],lista))
        else:
            r.append((lista[0], lista))
        return r
    else:
        if len(r) > 0:
            r.append((lista[0] + r[-1][0],lista))
        else:
            r.append((lista[0], lista))
        r = recursiva(lista[1:],r)
        return r

def maxsoma(lista):
    #lista = [1,2,3,4,-11,1,2,3,4,5]
    m = -9999
    chosen = []
    for i,x in enumerate(lista):
        r = [] # (soma,lista)
        maximo = -999999
        r = recursiva(lista[i:],r)
        if r[0][0] > maximo:
            maximo = r[0][0]
        for y in r:
            if y[0] > m:
                m = y[0]
                chosen = y[1]
        print(r)
    #print(r)
    print(chosen)

    return m