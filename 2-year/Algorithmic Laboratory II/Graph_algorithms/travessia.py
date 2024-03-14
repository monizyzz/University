def dijkstra(adj, o):
    pai = {}
    dist = {}
    dist[o] = 0
    orla = {o}
    while orla:
        v = min(orla, key=lambda x: dist[x])
        orla.remove(v)
        for d in adj[v]:
            if d not in dist:
                orla.add(d)
                dist[d] = float("inf")
            if dist[v] + adj[v][d] < dist[d]:
                pai[d] = v
                dist[d] = dist[v] + adj[v][d]
    return dist


def build(arestas):
    adj = {}
    for o, d, p in arestas:
        if o not in adj:
            adj[o] = {}
        if d not in adj:
            adj[d] = {}
        adj[o][d] = p
        adj[d][o] = p
    return adj


def travessia(mapa):
    lista = []
    tamanho_x = len(mapa)
    tamanho_y = len(mapa[0])
    for x in range(len(mapa)):
        for y in range(len(mapa[x])):
            ponto = (x, y)
            for xx, yy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                xn = xx + ponto[0]
                yn = yy + ponto[1]

                if 0 <= xn < tamanho_x and 0 <= yn < tamanho_y and abs(int(mapa[ponto[0]][ponto[1]]) - int(mapa[xn][yn])) <= 2:
                    custo = 1 + \
                        abs(int(mapa[ponto[0]][ponto[1]]) - int(mapa[xn][yn]))
                    lista.append((ponto, (xn, yn), custo))

    adj = build(lista)
    pontos_a_iterar = [x for x in adj.keys() if x[0] == 0]

    """ result = fw(build(lista))
        filtered_result = {k: v for k, v in result.items() if v != float('inf')}
        print(filtered_result) """

    minDist = 1000
    posicao = 10
    for i in range(len(pontos_a_iterar)):
        distancias = [h for d, h in dijkstra(adj, pontos_a_iterar[i]).items(
        ) if h != float("inf") and d[0] == tamanho_x - 1]
        if distancias:
            dist_atual = min(distancias)
        if dist_atual < minDist:
            minDist = dist_atual
            posicao = i
    custo_final = minDist
    inicio = pontos_a_iterar[posicao][1]
    print(custo_final)
    print(inicio)
    return (inicio, custo_final)


mapa = ["90999",
        "00000",
        "92909",
        "94909"]


travessia(mapa)