"""

Implemente uma função que recebe um número inteiro e retorna o número de maneiras de obter 
esse valor lançando um dado de 6 lados sucessivamente.

"""
dados = set()
memo = {}

def menos(num,r):
    global dados
    global memo
    string = str((num,r))
    if string in memo:
        for x in memo[string]:
            dados.add(x)
    else:
        if num == 0:
            dados.add(str(r))
        elif num == 1:
            r.append(1)
            r = menos(num-1,r)
        else:
            n = 1
            while n <= num and n <= 6:
                r = menos(num-n,r+[n])
                n += 1
    if string not in memo:
        memo[string] = set()
    memo[string].add(str(r))
    return r
    
def dado(soma):
    global dados
    dados = set()
    if soma <= 0:
        return 0
    menos(soma,[])
    print(dados)
    print(memo)
    return len(dados)