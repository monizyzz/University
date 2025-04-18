'''

Defina uma função que, dada uma lista de nomes de pessoas, devolva essa lista ordenada 
por ordem crescente do número de apelidos (todos menos o primeiro nome). No caso de pessoas com o mesmo número de apelidos,
devem ser listadas por ordem lexicográfica do nome completo.

'''

# 100%
def apelidos(nomes):
    nomes.sort(key = lambda t: (len(t.split()), t))
    
    return nomes