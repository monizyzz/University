"""

Implemente uma função que dada uma lista de papers (cada um representado por um triplo 
com a lista de autores, o título e o número de citações), devolve uma lista de pares com 
os autores e o respetivo h-index. Esta lista deve estar ordenada por ordem decrescente do 
h-index e em caso de empate por ordem alfabética dos autores.

O h-index de um autor é o maior número h tal que o autor tem h papers com pelo menos h citações.

"""

def listaAut(papers):
    r = set()
    for auths, name, cit in papers:
        for auth in auths:
            r.add(auth)
    return r

def setCit(papers,listaAut):
    r = {}
    for aut in listaAut:
        l = []
        for auths, name, cit in papers:
            for auth in auths:
                if aut == auth:
                    l.append(cit)
        r[aut] = l
    return r
    
def hCalc(citList):
    for i in range(999):
        print("i:",i)
        counter = 0
        menor = 9999999999
        for cit in citList: #contamos cit maior que r
            if cit >= i:
                if cit < menor:
                    menor = cit
                counter += 1
        
        print("counter:",counter)
        if counter >= i  and menor >= i: #vemos se counter (citações maiores q r) são >= a r, ou seja se pode ser h-index
            continue
        else:
            break
    return i-1

def hindex(papers):
    print(papers)
    lista = listaAut(papers)
    dic = setCit(papers,lista)
    r = []

    for aut in dic:
        print(aut,dic[aut])
        r.append((aut,hCalc(dic[aut])))
    
    r.sort(key = lambda x: (-x[1],x[0]))
    return r
    