'''

O objectivo deste problema é determinar quantos movimentos são necessários para 
movimentar um cavalo num tabuleiro de xadrez entre duas posições.
A função recebe dois pares de coordenadas, que identificam a origem e destino pretendido,
devendo devolver o número mínimo de saltos necessários para atingir o destino a partir da origem.
Assuma que o tabuleiro tem tamanho ilimitado.

'''

# 100%
def bfs(o,dest):
    dist = {}
    dist[o] = 0
    vis = {o}
    queue = [o]
    
    while queue:
        v = queue.pop(0)
        xo, yo = v[0], v[1]
        
        if v == dest:
            return dist[v]
        
        for i,j in [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]:
            xn,yn = xo + i, yo + j
            
            if (xn,yn) not in vis:
                vis.add((xn,yn))
                dist[(xn,yn)] = dist[v] + 1
                queue.append((xn,yn))
                
                if dest == (xn,yn):
                    return dist[dest]

def saltos(o,d):
    return bfs(o,d)