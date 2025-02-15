'''

Neste problema prentede-se que implemente uma função que calcula o rectângulo onde se movimenta um robot.

Inicialmente o robot encontra-se na posição (0,0) virado para cima e irá receber uma sequência de comandos numa string.
Existem quatro tipos de comandos que o robot reconhece:
  'A' - avançar na direcção para o qual está virado
  'E' - virar-se 90º para a esquerda
  'D' - virar-se 90º para a direita 
  'H' - parar e regressar à posição inicial virado para cima
  
Quando o robot recebe o comando 'H' devem ser guardadas as 4 coordenadas (minímo no eixo dos X, mínimo no eixo dos Y, máximo no eixo dos X, máximo no eixo dos Y) que definem o rectângulo 
onde se movimentou desde o início da sequência de comandos ou desde o último comando 'H'.

A função deve retornar a lista de todas os rectangulos (tuplos com 4 inteiros)

'''

# 100%
import math

def verCord(ant,nov):
    a,b,c,d = ant
    x,y,r = nov
    res = [a,b,c,d]
    if x < a:
        res[0] = x
    if y < b:
        res[1] = y
    if x > c:
        res[2] = x
    if y > d:
        res[3] = y
    return ((res[0],res[1],res[2],res[3]))
    
def robot(comandos):
    res = []
    pos = (0,0,90)
    ant = (0,0,0,0)
    for c in comandos:
        x,y,r = pos
        print(c)
        #print("x:",x,"| y:",y,"| r:",r)
        if c == 'A':
            pos = (x+int(math.cos(math.radians(r))),y + int(math.sin(math.radians(r))),r)   
            print(pos)
            ant = verCord(ant,pos)
        elif c == 'E':
            pos = (x,y,(r + 90))
        elif c == 'D':
            pos = (x,y,(r - 90))
        elif c == 'H':
            pos = (0,0,90)
            res.append(ant)
            ant = (0,0,0,0)
        else: 
            res.append(ant)
    return res