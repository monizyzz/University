'''

O objectivo deste problema é determinar o tamanho do maior continente de um planeta.
Considera-se que pertencem ao mesmo continente todos os países com ligação entre si por terra. 
Irá receber uma descrição de um planeta, que consiste numa lista de fronteiras, onde cada fronteira
é uma lista de países que são vizinhos entre si. 
A função deverá devolver o tamanho do maior continente.

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
    return vis
    
    
def aux(viz):
    adj = {}
    
    for cont in viz:
        for pais in cont:
            if pais not in adj:
                adj[pais] = set()
              
    return adj
    
def build(viz,adj):

    for pais in adj:
        for cont in viz:
            for p in cont:
                if pais != p and pais in cont:
                    adj[p].add(pais)
                    
    return adj

def maior(vizinhos):
    maxi = 0

    adj = aux(vizinhos)
    
    grafo = build(vizinhos, adj)
    print(grafo)
    
    for p in grafo:
        vis = bfs(grafo,p)
        if len(vis) > maxi:
            maxi = len(vis)
    
    return maxi