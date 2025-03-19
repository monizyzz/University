'''

Implemente uma função que calcula a área de um mapa que é acessível por
um robot a partir de um determinado ponto.
O mapa é quadrado e representado por uma lista de strings, onde um '.' representa
um espaço vazio e um '*' um obstáculo.
O ponto inicial consistirá nas coordenadas horizontal e vertical, medidas a 
partir do canto superior esquerdo.
O robot só consegue movimentar-se na horizontal ou na vertical. 

'''

# 100%
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
    return len(vis)
    
    
def build(mapa):
    adj = {}
    
    for y,row in enumerate(mapa):
        for x,char in enumerate(row):
            if char != '*' and (x,y) not in adj:
                adj[(x,y)] = set()
                
                for i,j in [(-1,0), (0,-1), (1,0), (0,1)]:
                    xn, yn = x + i, y + j
                    
                    if 0 <= xn < len(mapa) and 0 <= yn < len(mapa) and mapa[yn][xn] != '*':
                        adj[(x,y)].add((xn,yn))   
            
    return adj

def area(p,mapa):
    adj = build(mapa)
    return bfs(adj,p)