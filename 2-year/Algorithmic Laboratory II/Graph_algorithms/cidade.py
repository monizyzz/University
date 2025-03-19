'''

Podemos usar um (multi) grafo para representar um mapa de uma cidade: 
cada nó representa um cruzamento e cada aresta uma rua.
Pretende-se que implemente uma função que calcula o tamanho de uma cidade, 
sendo esse tamanho a distância entre os seus cruzamentos mais afastados.

A entrada consistirá numa lista de nomes de ruas (podendo assumir que os 
nomes de ruas são únicos). Os identificadores dos cruzamentos correspondem a 
letras do alfabeto, e cada rua começa (e acaba) no cruzamento 
identificado pelo primeiro (e último) caracter do respectivo nome.

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
 
 
def build(ruas):
    adj = {}
    
    for rua in ruas:
        o, d = rua[0], rua[-1]
        custo = len(rua)
        
        if o not in adj:
            adj[o] = {}
            
        if d not in adj:
            adj[d] = {}
            
        
        if d not in adj[o] or adj[o][d] > custo:
            adj[o][d] = custo
            
            
        if o not in adj[d] or adj[d][o] > custo:
            adj[d][o] = custo
        
    return adj
    
            
def tamanho(ruas):
    grafo = build(ruas)
    
    res = 0
    
    for c in fw(grafo).values():
        for custo in c.values():
            if custo > res:
                res = custo
    
    return res