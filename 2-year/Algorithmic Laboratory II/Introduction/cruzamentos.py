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

def cruzamentos(ruas):
    dic = {}  

    for rua in ruas:
        first = rua[0]
        last = rua[-1]
    
        if first == last:
            if first not in dic.keys():
                dic[first] = 0
            dic[first] += 1
        else:
            if first not in dic.keys():
                dic[first] = 0
            dic[first] += 1
        
            if last not in dic.keys():
              dic[last] = 0
            dic[last] += 1
        
    cruzamento = list(dic.items())

    cruzamento.sort(key=lambda x: (x[1], x[0]))

    return cruzamento