'''

Neste problem pretende-se que defina uma função que, dada uma string com palavras, 
devolva uma lista com as palavras nela contidas ordenada por ordem de frequência,
da mais alta para a mais baixa. Palavras com a mesma frequência devem ser listadas 
por ordem alfabética.

'''

# 100%
def frequencia(texto):
    dic = {}
    x = texto.split()
    for pal in x:
        if pal not in dic:
            dic[pal] = 1    
        else:
            dic[pal] += 1
    
    dic = sorted(dic.items(), key = lambda x:(-1*x[1],x[0]))
    
    r = []
    for x,y in dic:
        r.append(x)
    
    return r