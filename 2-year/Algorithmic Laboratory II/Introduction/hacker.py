"""

Um hacker teve acesso a um log de transações com cartões de
crédito. O log é uma lista de tuplos, cada um com os dados de uma transação,
nomedamente o cartão que foi usado, podendo alguns dos números estar
ocultados com um *, e o email do dono do cartão.

Pretende-se que implemente uma função que ajude o hacker a 
reconstruir os cartões de crédito, combinando os números que estão
visíveis em diferentes transações. Caso haja uma contradição nos números 
visíveis deve ser dada prioridade à transção mais recente, i.é, a que
aparece mais tarde no log.

A função deve devolver uma lista de tuplos, cada um com um cartão e um email,
dando prioridade aos cartões com mais digitos descobertos e, em caso de igualdade
neste critério, aos emails menores (em ordem lexicográfica).

"""

def hacker(log):
    final = []
    
    for x,y in log:
        if y not in [y[1] for y in final]:
            final.append((x,y))
        else:
            for i,(a,b) in enumerate(final):
                if y == b:
                    final[i] = ("".join([c if c != '*' else s for s, c in zip(a, x)]), b)
                    
    final.sort(key = lambda x: x[1])
    final.sort(key = lambda y: y[0].count('*'))
    
    return final