'''

O objectivo deste problema é determinar quantos movimentos são necessários para 
movimentar um cavalo num tabuleiro de xadrez entre duas posições.
A função recebe dois pares de coordenadas, que identificam a origem e destino pretendido,
devendo devolver o número mínimo de saltos necessários para atingir o destino a partir da origem.
Assuma que o tabuleiro tem tamanho ilimitado.

'''

def bfs(o,dest):
    pai = {}
    vis = {o}
    queue = [o]
    
    while queue:
        v = queue.pop(0)
        x, y = v
        
        if v == dest:
            return pai[v]
        
        for dx, dy in [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]:
            nx, ny = x + dx, y + dy
            if (nx, ny) not in vis:
                vis.add((nx, ny))
                pai[(nx, ny)] = pai.get(v, 0) + 1
                queue.append((nx, ny))
            

def saltos(o,d):
    if o == d:
        return 0
        
    count = bfs(o,d)
    
    return count