'''

Neste problem pretende-se que defina uma função que, dada uma string com palavras, 
devolva uma lista com as palavras nela contidas ordenada por ordem de frequência,
da mais alta para a mais baixa. Palavras com a mesma frequência devem ser listadas 
por ordem alfabética.

'''

def frequencia(texto):
    dic = {}
    
    palavras = texto.split()
    
    for char in palavras:
        if char not in dic:
            dic[char] = 0
        dic[char] += 1
    
    res = list(dic.items())    
    
    res.sort(key= lambda j: j[0])
    res.sort(key= lambda x: x[1], reverse=True)
            
    final = []        
    for o in res:
        final.append(o[0])
        
    """ 
    res = [word for word,num in final]
    """
    
    return final