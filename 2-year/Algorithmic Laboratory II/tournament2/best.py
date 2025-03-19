'''
Implement a function that receives a directed graph and computes the node from which more nodes can be reached.
Nodes are identified by a single letter.
The graph is defined by a list of edges, where each edge is described by a string with two nodes separated by 
one of the following strings depending on the direction the edge can be traversed: '->', '<-', '<>'.
If there are multiple nodes that can reach the same maximum number of nodes, the function should return the node 
with the lexicographically smallest name.

Implemente uma função que recebe um grafo orientado e calcula o nó a partir do qual mais nós podem ser alcançados.
Os nós são identificados por uma única letra.
O grafo é definido por uma lista de arestas, onde cada aresta é descrita por uma string com dois nós separados por 
uma das seguintes strings, dependendo da direção que a aresta pode ser percorrida: '->', '<-', '<>'.
Se houver múltiplos nós que podem alcançar o mesmo número máximo de nós, a função deve retornar o nó 
com o nome lexicograficamente menor.
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
    return len(vis)

def init(edges):
    r = {}
    for edge in edges:
        r[edge[0]] = set()
        r[edge[3]] = set()
    return r

def build(edges):
    r = init(edges)
    for edge in edges:
        if edge[1] == "-" and edge[2] == ">": #direita
            r[edge[0]].add(edge[3])
        elif edge[1] == "<" and edge[2] == "-":
            r[edge[3]].add(edge[0])
        elif edge[1] == "<" and edge[2] == ">":
            r[edge[0]].add(edge[3])
            r[edge[3]].add(edge[0])
    return r
    
    
def best(edges):
    adj = build(edges)
    maior = ("",-1)
    for v in adj:
        r = bfs(adj,v)
        if r > maior[1]:
            maior = (v,r)
        elif r == maior[1]:
            l = [maior[0],v]
            l.sort()
            maior = (l[0],r)
    
    return maior[0]