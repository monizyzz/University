'''

Implemente uma função que calcula um dos caminhos mais curtos para atravessar
um labirinto. O mapa do labirinto é quadrado e representado por uma lista 
de strings, onde um ' ' representa um espaço vazio e um '#' um obstáculo.
O ponto de entrada é o canto superior esquerdo e o ponto de saída o canto
inferior direito. A função deve devolver uma string com as instruções para
atravesar o labirinto. As instruções podem ser 'N','S','E','O'.

'''

def bfs(adj, o, d):
    pai = {}
    vis = {o}
    queue = [o]
    while queue:
        v = queue.pop(0)
        if v == d:
            return pai
        
        xo, yo = v
        
        for d in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            xn, yn  = xo + d[0], yo + d[1]
            
            if (xn, yn) not in vis and 0 <= xn < len(adj) and 0 <= yn < len(adj[0]) and adj[xn][yn] != '#':
                vis.add((xn, yn))
                pai[(xn, yn)] = v
                queue.append((xn, yn))
    return pai
    
    
def direcoes(possiveis, dest):
    lista = []
    atual = dest
    
    while atual in possiveis.keys():
        pai = possiveis[atual]
        
        if pai[0] == atual[0] and pai[1] < atual[1]: 
            lista.append('E')
            
        elif pai[0] == atual[0] and pai[1] > atual[1]:
            lista.append('O')
            
        elif pai[1] == atual[1] and pai[0] < atual[0]:
            lista.append('S')
            
        elif pai[1] == atual[1] and pai[0] > atual[0]:
            lista.append('N')
        
        atual = pai
    
    return "".join(lista[::-1])
    
    

def caminho(mapa):
    o = (0,0)
    d = (len(mapa)-1, len(mapa)-1)
    if o == d:
        return ""
    
    possiveis = bfs(mapa, o, d)
    res = direcoes(possiveis, d)
    
    return res