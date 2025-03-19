'''

Implemente uma função que calcula o preço mais barato para fazer uma viagem de
autocarro entre duas cidades. A função recebe (para além das duas cidades) uma
lista de rotas de autocarro, onde cada rota é uma sequência de cidades por onde
passa o autocarro, intercalada com o custo para viajar entre cada par de cidades.
Assuma que cada rota funciona nos dois sentidos.

'''

# 100%
def fw(adj):
    dist = {}
    for o in adj:
        dist[o] = {}
        for d in adj:
            if o == d:
                dist[o][d] = 0
            elif d in adj[o]:
                dist[o][d] = adj[o][d]
            else:
                dist[o][d] = float("inf")
    for k in adj:
        for o in adj:
            for d in adj:
                if dist[o][k] + dist[k][d] < dist[o][d]:
                    dist[o][d] = dist[o][k] + dist[k][d]
    return dist
    
def build(rotas):
    grafo = {}
    
    for rota in rotas:
        for i,c in enumerate(rota):
            if type(c) == str:
                if c not in grafo:
                    grafo[c] = {}
                    
                if len(rota)-1 != i and rota[i+2] not in grafo:
                    grafo[rota[i+2]] = {}
                    
                if len(rota)-1 != i:
                        grafo[c][rota[i+2]] = rota[i+1] # custo
                        grafo[rota[i+2]][c] = rota[i+1]
                    
    return grafo



def viagem(rotas,o,d):
    if o == d:
        return 0
    
    grafo = build(rotas)
    
    r = fw(grafo)

    return r[o][d]

# or

def dijkstra(adj,o):
    pai = {}
    dist = {}
    dist[o] = 0
    orla = {o}
    while orla:
        v = min(orla,key=lambda x:dist[x])
        orla.remove(v)
        for d in adj[v]:
            if d not in dist:
                orla.add(d)
                dist[d] = float("inf")
            if dist[v] + adj[v][d] < dist[d]:
                pai[d] = v
                dist[d] = dist[v] + adj[v][d]
    return dist
    
def build(rotas):
    grafo = {}
    
    for rota in rotas:
        for i,c in enumerate(rota):
            if type(c) == str:
                if c not in grafo:
                    grafo[c] = {}
                    
                if i+2 < len(rota) and rota[i+2] not in grafo:
                    grafo[rota[i+2]] = {}
                    
                if i+2 < len(rota):
                    grafo[c][rota[i+2]] = rota[i+1] # custo
                    grafo[rota[i+2]][c] = rota[i+1]
                    
    return grafo



def viagem(rotas,o,d):
    if o == d:
        return 0
    
    grafo = build(rotas)
    
    r = dijkstra(grafo,o)

    return r[d]