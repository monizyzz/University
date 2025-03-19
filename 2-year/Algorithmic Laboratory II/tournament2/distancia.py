'''
Implement a function that receives a map and two cities, and returns the minimum distance between the two cities.
The map is defined as a list of strings, where each string represents a row of the map.
The map is a grid of squares, where each square can be a city (a letter) or a road ('-', '|', or '+', depending on 
the direction it can be traversed). A city can be traversed in any direction.

Implemente uma função que recebe um mapa e duas cidades, e retorna a distância mínima entre as duas cidades.
O mapa é definido como uma lista de strings, onde cada string representa uma linha do mapa.
O mapa é uma grelha de quadrados, onde cada quadrado pode conter uma cidade (uma letra) ou uma estrada 
('-' '|' ou '+', dependendo da direção que pode ser percorrida). Uma cidade pode ser percorrida em qualquer direção.
'''

def bfs(adj,o,dest):
    dist = {o:0}
    vis = {o}
    queue = [o]
    while queue:
        v = queue.pop(0)
        for d in adj[v]:
            if d not in vis:
                vis.add(d)
                dist[d] = dist[v] + 1
                queue.append(d)
                if d == dest:
                    return dist[d]
    return vis

def init(map):
    r = {}
    for y, row in enumerate(map):
        for x, c in enumerate(row):
            if c != " ":
                r[(x,y)] = set()
    return r

def direction(map,coord,c,r):
    x,y = coord
    lenY = len(map)
    lenX = len(map[0])
    
    if c == "-" : # se houver traço fronteira adicionar
        if x-1 >= 0 and map[y][x-1] != " ": 
            r[(x,y)].add((x-1,y)) #esquerda
        if x+1 < lenX and map[y][x+1] != " ":
            r[(x,y)].add((x+1,y)) #direita
    elif c == "|":
        if y-1 >= 0 and map[y-1][x] != " ":
            r[(x,y)].add((x,y-1))
        if y+1 < lenY and map[y+1][x] != " ":
            r[(x,y)].add((x,y+1))
    elif c == "+" or c != " ":
        if x+1 < lenX and map[y][x+1] != " ":
            r[(x,y)].add((x+1,y))
        if x-1 >= 0 and map[y][x-1] != " ":
            r[(x,y)].add((x-1,y))
        if y+1 < lenY and map[y+1][x] != " ":
            r[(x,y)].add((x,y+1))
        if y-1 >= 0 and map[y-1][x] != " ":
            r[(x,y)].add((x,y-1))
    return r

def build(map):
    built = init(map)
    
    for y, row in enumerate(map):
        for x, c in enumerate(row):
            if c != " ":
                built = direction(map,(x,y),c,built)
    return built    
    
def finder(map,d):
    for y, row in enumerate(map):
        for x, c in enumerate(row):
            if c == d:
                return (x,y)
    return -1
    
def distancia(map,a,b):
    if map == "":
        return 0
    elif a == b:
        return 0
    o = finder(map,a)
    d = finder(map,b)
    mapa = build(map)
    return bfs(mapa,o,d)