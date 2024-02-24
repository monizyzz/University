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

def robot(comandos):
    movs = {
        0: (0,1),    # norte
        1: (1,0),    # este
        2: (0,-1),   # sul
        3: (-1,0),   # oeste
    }
    
    direcao = 0
    mov = [0,0] # x,y
    minX = 0
    minY = 0
    maxX = 0
    maxY = 0
    final = []
    
    for i in comandos:
        if i == 'A':
            mov[0] += movs[direcao][0] 
            mov[1] += movs[direcao][1]
            
            if mov[0] > maxX:
                maxX = mov[0]
            elif mov[0] < minX:
                minX = mov[0]
                
            if mov[1] > maxY:
                maxY = mov[1]
            elif mov[1] < minY:
                minY = mov[1]
                
        
        elif i == 'E':
            direcao = (direcao - 1) % 4            
        
        elif i == 'D':
            direcao = (direcao + 1) % 4
        
        else:
            final.append((minX, minY, maxX, maxY))
            direcao = 0
            mov[0] = 0
            mov[1] = 0
            minX = 0
            minY = 0
            maxX = 0
            maxY = 0
            
            
    return final