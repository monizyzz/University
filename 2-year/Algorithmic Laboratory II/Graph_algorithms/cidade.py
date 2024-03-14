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
    
    
def build(arestas):
    adj = {}
    
    for o,d,p in arestas:
        if o != d:
            if o not in adj:
                adj[o] = {}
            if d not in adj:
                adj[d] = {}
            
            if d not in adj[o].keys() or adj[o][d] > p:
                adj[o][d] = p
                adj[d][o] = p
        
    return adj
    
 
def tamanho(ruas):
    lista = []
    maior = 0
    
    for rua in ruas:
        lista.append((rua[0],rua[-1],len(rua)))
        
    adj = build(lista)
    
    dist = fw(adj)
    
    for letra in dist:
        
        for atual in dist[letra]:
            tamanho = dist[letra][atual]
            
            if maior < tamanho:
                maior = tamanho
    
    return maior