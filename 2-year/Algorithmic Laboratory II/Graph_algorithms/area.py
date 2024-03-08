'''

Implemente uma função que calcula a área de um mapa que é acessível por
um robot a partir de um determinado ponto.
O mapa é quadrado e representado por uma lista de strings, onde um '.' representa
um espaço vazio e um '*' um obstáculo.
O ponto inicial consistirá nas coordenadas horizontal e vertical, medidas a 
partir do canto superior esquerdo.
O robot só consegue movimentar-se na horizontal ou na vertical. 

'''

def bfs(adj,o):
    pai = {}
    vis = {o}
    queue = [o]
    while queue:
        v = queue.pop(0)
        for d in adj[v]:
            if d not in vis:
                vis.add(d)
                pai[d] = v
                queue.append(d)
    return vis
    
def verifica_caminhos(mapa,i,j):
    array = []
    

    if j+1 < len(mapa[0]) and mapa[i][j+1] == '.':
        array.append((i,j+1))
    
    if j-1 >= 0 and mapa[i][j-1] == '.':
            array.append((i,j-1))
    
    if i+1 < len(mapa) and mapa[i+1][j] == '.':
            array.append((i+1,j))
    
    if i-1 >= 0 and mapa[i-1][j] == '.':
            array.append((i-1,j))
    
    return array


def area(p,mapa):
    adj = {}
    
    if mapa == []:
        return 0
        
    for i in range(len(mapa)):
        for j in range(len(mapa[i])):
            ponto = (i,j)
            if ponto not in adj and mapa[i][j] == '.':
                adj[ponto] = set()
                caminhos = verifica_caminhos(mapa,i,j)
                adj[ponto] = caminhos
                
                

    d = bfs(adj,p)
    
    return len(d)