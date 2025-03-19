'''

O número de Erdos é uma homenagem ao matemático húngaro Paul Erdos,
que durante a sua vida escreveu cerca de 1500 artigos, grande parte deles em 
parceria com outros autores. O número de Erdos de Paul Erdos é 0. 
Para qualquer outro autor, o seu número de Erdos é igual ao menor 
número de Erdos de todos os seus co-autores mais 1. Dado um dicionário que
associa artigos aos respectivos autores, implemente uma função que
calcula uma lista com os autores com número de Erdos menores que um determinado 
valor. A lista de resultado deve ser ordenada pelo número de Erdos, e, para
autores com o mesmo número, lexicograficamente.

'''

# 100%
def bfs(adj,o):
    dist = { o: 0 }
    vis = {o}
    queue = [o]
    while queue:
        v = queue.pop(0)
        for d in adj[v]:
            if d not in vis:
                vis.add(d)
                dist[d] = dist[v] + 1
                queue.append(d)
    return dist

def aux(artigos):
    adj = {}
    
    for auths in artigos.values():
        for auth in auths:
            if auth not in adj:
                adj[auth] = set()
    return adj

def build(adj, artigos):
    grafo = adj
    
    for a in adj:
        for auths in artigos.values():
            for auth in auths:
                if a in auths and a != auth:
                    grafo[a].add(auth)
    
    return grafo

def erdos(artigos,n):
    if artigos == {} or n <= 0:
        return ["Paul Erdos"]
    
    adj = aux(artigos)
    grafo = build(adj, artigos)
    
    lista = bfs(grafo, "Paul Erdos")
    
    res = sorted(lista.items(), key = lambda x: (x[1], x[0]))
    
    final = [a for a,c in res if c <= n]
    
    return final