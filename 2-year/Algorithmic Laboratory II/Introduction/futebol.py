'''

Implemente uma função que calcula a tabela classificativa de um campeonato de
futebol. A função recebe uma lista de resultados de jogos (tuplo com os nomes das
equipas e os respectivos golos) e deve devolver a tabela classificativa (lista com 
as equipas e respectivos pontos), ordenada decrescentemente pelos pontos. Em
caso de empate neste critério, deve ser usada a diferença entre o número total
de golos marcados e sofridos para desempatar, e, se persistir o empate, o nome
da equipa.

'''

# 100%
def tabela(jogos):
    dic = {}
    for jogo in jogos:
        if jogo[0] not in dic:
            dic[jogo[0]] = [0,0]
        if jogo[2] not in dic:
            dic[jogo[2]] = [0,0]
    
    for jogo in jogos:
        casa,gc,fora,gf = jogo
        if (gc > gf):
            dic[casa][0] += 3
            dic[casa][1] += gc - gf
            dic[fora][1] -= gc - gf
        elif (gc == gf):
            dic[casa][0] += 1
            dic[fora][0] += 1
        else:
            dic[fora][0] += 3
            dic[fora][1] += gf - gc
            dic[casa][1] -= gf - gc
        
    
    dic = sorted(dic.items(), key = lambda x: (-1*x[1][0], -1*x[1][1], x[0]))
    
    r = []
    for x,y in dic:
        r.append((x,y[0]))
        
    """ Or
    r = [(x, y[0]) for x,y in clas]
    """
    
    return r