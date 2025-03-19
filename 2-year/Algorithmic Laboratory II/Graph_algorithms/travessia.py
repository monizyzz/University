'''

Implemente uma função que calcula o menor custo de atravessar uma região de
Norte para Sul. O mapa da região é rectangular, dado por uma lista de strings,
onde cada digito representa a altura de cada ponto. Só é possível efectuar 
movimentos na horizontal ou na vertical, e só é possível passar de um ponto
para outro se a diferença de alturas for inferior ou igual a 2, sendo o custo 
desse movimento 1 mais a diferença de alturas. O ponto de partida (na linha
mais a Norte) e o ponto de chegada (na linha mais a Sul) não estão fixados à
partida, devendo a função devolver a coordenada horizontal do melhor
ponto para iniciar a travessia e o respectivo custo. No caso de haver dois pontos
com igual custo, deve devolver a coordenada mais a Oeste.

'''

# 100%
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

def build(mapa):
    adj = {}
    
    for y,row in enumerate(mapa):
        for x,char in enumerate(row):
            if (x,y) not in adj:
                adj[(x,y)] = {}
            
            for i,j in [(0,-1), (-1,0), (0,1), (1,0)]:
                nx, ny = x + i, y + j
                
                if 0 <= nx < len(row) and 0 <= ny < len(mapa) and abs(int(char) - int(mapa[ny][nx])) <= 2:
                    adj[(x,y)][(nx,ny)] = 1 + abs(int(char) - int(mapa[ny][nx]))
                    
    return adj


def travessia(mapa):
    grafo = build(mapa)
    
    altura = len(mapa)
    largura = len(mapa[0])
    
    origens = [(x,0) for x in range(largura)]
    
    posInicial = -1
    menorCusto = float("inf")
    
    
    for o in origens:
        dist = dijkstra(grafo, o)
    
        for d, custo in dist.items():
            if d[1] == altura - 1:
                if menorCusto > custo:
                    menorCusto = custo
                    posInicial = o[0]
                elif menorCusto == custo and o[0] < posInicial:
                    posInicial = o[0]
    
    return (posInicial, menorCusto)