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

# 100%
# Cria o numero para o email dado
def singleList(log):
    mails = []
    for num, mail in log:
        if mail not in mails:
            mails.append(mail)
    return mails


def numSplitter(log,mail):
    r = ["*"] * 16
    n = 0
    for num, ma in log:
        for i in range(0,len(num)):
            if ma == mail and num[i] != '*':
                if r[i] == '*':
                    n += 1
                r[i] = num[i]
    return ("".join(r),mail,n)

def hacker(log):
    res = []
    mailList = singleList(log)
    for mail in mailList:
        res.append(numSplitter(log,mail))
    res.sort(key = lambda x:(-x[2],x[1]))
    result = []
    for x,y,z in res:
        result.append((x,y))
    return result