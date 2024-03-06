'''

O objectivo deste problema é determinar o tamanho do maior continente de um planeta.
Considera-se que pertencem ao mesmo continente todos os países com ligação entre si por terra. 
Irá receber uma descrição de um planeta, que consiste numa lista de fronteiras, onde cada fronteira
é uma lista de países que são vizinhos entre si. 
A função deverá devolver o tamanho do maior continente.

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
    return pai

# from Root.src.grafos import bfs

def maior(vizinhos):
    adj = {}
    big = 0
    
    if len(vizinhos) == 0:
        return 0
    
    for continente in vizinhos:
        if continente[0] not in adj:
            adj[continente[0]] = set()
        for pais in continente[1:]:
            if pais not in adj:
                adj[pais] = set()
                
            adj[continente[0]].add(pais)
            adj[pais].add(continente[0])


    for x in adj.keys():
        p = bfs(adj, x)
        
        if big < len(p):
            big = len(p)
    
    return big + 1

# or 

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
    return pai



def maior(vizinhos):
    if (len(vizinhos)== 0): return 0
    adj = {}
    
    for continentes in vizinhos:
        for i in range(len(continentes) - 1):
            if continentes[i] not in adj:
                adj[continentes[i]] = set()
            if continentes[i+1] not in adj:
                adj[continentes[i+1]] = set()
            adj[continentes[i]].add(continentes[i+1])
            adj[continentes[i+1]].add(continentes[i])
    maior = 0 
    for x in adj:
        d = bfs(adj,x)
        if(len(d) > maior):
          maior = len(d) 


    return maior + 1