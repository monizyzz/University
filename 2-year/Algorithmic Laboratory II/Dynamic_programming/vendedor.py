"""

Um vendedor ambulante tem que decidir que produtos levará na sua próxima viagem.
Infelizmente, tem um limite de peso que pode transportar e, tendo isso em atenção, 
tem que escolher a melhor combinação de produtos a transportar dentro desse limite 
que lhe permitirá ter a máxima receita.

Implemente uma função que, dado o limite de peso que o vendedor pode transportar, 
e uma lista de produtos entre os quais ele pode escolher (assuma que tem à sua 
disposição um stock ilimitado de cada produto), devolve o valor de receita máximo
que poderá obter se vender todos os produtos que escolher transportar, e a lista
de produtos que deverá levar para obter essa receita (incluindo repetições, 
caso se justifique), ordenada alfabeticamente.

Cada produto consiste num triplo com o nome, o valor, e o peso.

Caso haja 2 produtos com a mesma rentabilidade por peso deverá dar prioridade 
aos produtos que aparecem primeiro na lista de entrada.

"""

# 50%
def stats(produtos):
    r = []
    for i,prod in enumerate(produtos):
        vpp = prod[1]/prod[2]
        r.append((prod[0],prod[2],vpp,i))
    r.sort(key=lambda x: (-x[2],i,x[1],x[0]))
    return r

# Estratégia
# Ver quais os melhores por valor/peso
# Adicionar o maximo possivel do melhor, depois fazer algoritmo para o resto

def vendedor(capacidade,produtos):
    prodOrder = stats(produtos) # ('nome',peso,vpp)
    v = 0
    r = []
    for pO in prodOrder:
        peso = pO[1]
        val = peso*pO[2]
        if peso == capacidade:
            return (pO[0],val)
        elif peso < capacidade:
           rep = capacidade // peso
           v += val*rep
           capacidade -= peso * rep
           for i in range(0,rep):
               r.append(pO[0])
    return (v,r)