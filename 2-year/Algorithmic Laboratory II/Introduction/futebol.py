'''

Implemente uma função que calcula a tabela classificativa de um campeonato de
futebol. A função recebe uma lista de resultados de jogos (tuplo com os nomes das
equipas e os respectivos golos) e deve devolver a tabela classificativa (lista com 
as equipas e respectivos pontos), ordenada decrescentemente pelos pontos. Em
caso de empate neste critério, deve ser usada a diferença entre o número total
de golos marcados e sofridos para desempatar, e, se persistir o empate, o nome
da equipa.

'''

def tabela(jogos):
    classificacao = {}
    
    for jogo in jogos:
        casa, marcados_casa, visitante, marcados_visitante = jogo
        
        if casa not in classificacao:
            classificacao[casa] = {"pontos": 0, "marcados": 0, "sofridos": 0, "diferenca_golos": 0}
            
        if visitante not in classificacao:
            classificacao[visitante] = {"pontos": 0, "marcados": 0, "sofridos": 0, "diferenca_golos": 0}
            
        if marcados_casa == marcados_visitante:
            classificacao[casa]["pontos"] += 1
            classificacao[visitante]["pontos"] += 1
        elif marcados_casa > marcados_visitante:
            classificacao[casa]["pontos"] += 3
        elif marcados_casa < marcados_visitante:
            classificacao[visitante]["pontos"] += 3
            
        classificacao[casa]["marcados"] += marcados_casa
        classificacao[casa]["sofridos"] += marcados_visitante
        classificacao[casa]["diferenca_golos"] += marcados_casa - marcados_visitante
        
        classificacao[visitante]["marcados"] += marcados_visitante
        classificacao[visitante]["sofridos"] += marcados_casa
        classificacao[visitante]["diferenca_golos"] += marcados_visitante - marcados_casa
        
    clubes = []
        
    for x, y in classificacao.items():
        clubes.append((x, y["pontos"], y["diferenca_golos"]))
    
    clubes.sort()
    clubes.sort(key= lambda x: (x[1], x[2]), reverse=True)
                
    res = [(team, pontos) for team, pontos, _ in clubes]
    
    return res