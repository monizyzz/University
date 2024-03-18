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

def erdos(artigos,n):
    vis = {"Paul Erdos":0}
    queue = ["Paul Erdos"]
    
    while queue:
        v = queue.pop(0)
        for autores in artigos:
            for autor in artigos[autores]:
                if v == autor:
                    for x in artigos[autores]:
                        if x not in vis:
                            vis[x] = vis[v] + 1
                            queue.append(x)

    lista = [(x,y) for x,y in vis.items() if y <= n]
    lista.sort(key=lambda x: (x[1],x[0]))
    final = [x[0] for x in lista]
    return final