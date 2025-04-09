"""

Implemente uma função que determina qual a probabilidade de um robot regressar 
ao ponto de partido num determinado número de passos. Sempre que o robot dá um 
passo tem uma determinada probabilidade de seguir para cima ('U'), baixo ('D'), 
esquerda ('L') ou direita ('R'). A função recebe o número de passos que o 
robot vai dar e um dicionário com probabilidades de se movimentar em cada uma
das direcções (as chaves são os caracteres indicados entre parêntesis).
O resultado deve ser devolvido com a precisao de 2 casas decimais.

"""

# 100%
newProbs = {}
memo = {} # key = (pos,passos), value = prob

def strToPos(char):
    if char == 'D':
        return (0,-1)
    elif char == 'U':
        return (0,1)
    elif char == 'L':
        return (-1,0)
    elif char == 'R':
        return (1,0)

def calc(pos,passos):
    r = 0
    x, y = pos
    global memo
    if (pos, passos) in memo:
        r = memo[(pos,passos)]
    else:
        if passos == 0 and pos == (0,0):
            r = 1
        elif passos == 0 and pos != (0,0):
            r = 0
        elif x + y > passos:
            r = 0
        else:
            u1 = newProbs[(0,1)]*calc((x,y+1),passos-1)
            u2 = newProbs[(1,0)]*calc((x+1,y),passos-1)
            u3 = newProbs[(0,-1)]*calc((x,y-1),passos-1)
            u4 = newProbs[(-1,0)]*calc((x-1,y),passos-1)
            r = (u1 + u2 + u3 + u4)
    memo[(pos,passos)] = r
    return r

def probabilidade(passos,probs):
    global newProbs
    global memo
    newProbs = {}
    memo = {}
    for prob in probs:
        a = strToPos(prob)
        newProbs[a] = probs[prob]
    print("newProbs:",newProbs)
    if passos % 2 == 1:
        return 0
    r = calc((0,0),passos)
    return round(r * 100) / 100