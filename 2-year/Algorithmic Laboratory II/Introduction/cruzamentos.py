'''
Podemos usar um (multi) grafo para representar um mapa de uma cidade: 
cada nó representa um cruzamento e cada aresta uma rua.

Pretende-se que implemente uma função que lista os cruzamentos de uma cidade 
por ordem crescente de criticidade: um cruzamento é tão mais crítico quanto 
maior o número de ruas que interliga.

A entrada consistirá numa lista de nomes de ruas (podendo assumir que os nomes de ruas são únicos). 
Os identificadores dos cruzamentos correspondem a letras do alfabeto, e cada rua começa (e acaba) no cruzamento 
identificado pelo primeiro (e último) caracter do respectivo nome.

A função deverá retornar uma lista com os nomes dos cruzamentos por ordem crescente de criticidade, 
listando para cada cruzamento um tuplo com o respectivo identificador e o número de ruas que interliga.
Apenas deverão ser listados os cruzamentos que interliguem alguma rua, e os cruzamentos com o mesmo 
nível de criticidade deverão ser listados por ordem alfabética.

'''

# 100%
def cruzamentos(ruas):
    dic = {}
    for rua in ruas:
        if rua[0] not in dic:
            dic[rua[0]] = 1
        else:
            dic[rua[0]] += 1
            
        if rua[-1] not in dic:
            dic[rua[-1]] = 1
        else:
            if not(rua[0] == rua[-1]):
                dic[rua[-1]] += 1
            
    dic = sorted(dic.items(), key = lambda x: (x[1],x[0]))
    
    """ Or
    r = [(c,n) for c,n in dic.items()]
    
    r.sort(key = lambda x: (x[1],x[0]))
    """

    return dic