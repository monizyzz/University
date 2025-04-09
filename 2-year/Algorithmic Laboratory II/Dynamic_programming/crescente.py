"""

Implemente uma função que dada uma sequência de inteiros, determinar o 
comprimento da maior sub-sequência (não necessariamente contígua) que se 
encontra ordenada de forma crescente.

Sugere-se que comece por implementar uma função auxiliar recursiva que determina 
o comprimento da maior sub-sequência crescente que inclui o primeiro elemento
da sequência, sendo o resultado pretendido o máximo obtido aplicando esta
função a todos os sufixos da sequência de entrada.

"""

# 80%
memo = {}

def calc(lista,prev):
    global memo
    listStr = str(([prev] + lista))
    if listStr in memo:
        return memo[listStr]
    elif lista != []:
        r = []
        if lista[0] >= prev:
            u1 = calc(lista[1:],lista[0]) # adicionamos
            u2 = calc(lista[1:],prev) # não adicionamos
            if len(u1) >= len(u2):
                r = [lista[0]] + u1
            else:
                r = u2
        else:
            r += calc(lista[1:],prev)
        
        memo[listStr] = r
        return r
    return []

def crescente(lista):
    maior = 0
    for i in range(len(lista)):
        x = calc(lista[(i+1):],lista[i])
        leng = len(x) + 1
        if leng > maior:
            maior = leng
    
    return maior