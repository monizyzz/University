"""

Implemente uma função que calcula o horário de uma turma de alunos.
A função recebe dois dicionários, o primeiro associa a cada UC o
respectivo horário (um triplo com dia da semana, hora de início e
duração) e o segundo associa a cada aluno o conjunto das UCs em
que está inscrito. A função deve devolver uma lista com os alunos que
conseguem frequentar todas as UCs em que estão inscritos, indicando
para cada um desses alunos o respecto número e o número total de horas
semanais de aulas. Esta lista deve estar ordenada por ordem decrescente
de horas e, para horas idênticas, por ordem crescente de número.

"""

# 80%
#verifica se alguma uc colide, para depois poder facilmente descartar o aluno como horario invalido se tiver ucs que estejam numa lista
def colis(ucs):
    #print("ucs:",ucs)
    res = []
    for uc in ucs:
        main = ucs[uc]
        for uc2 in ucs:
            if uc2 != uc:
                sec = ucs[uc2] # [0][1][2] -> (dia,inicio,fim)
                if (sec[0] == main[0]) and ((sec[1] < main[1]+main[2] and sec[1] > main[1]) or (sec[1]+sec[2] > main[1] and sec[1]+sec[2] < main[1]+main[2])):
                    res.append([uc,uc2])
    return res

def cols(col,ucsAl):
    r = 0
    for ucsCol in col:
        for ucCol in ucsCol:
            for ucAl in ucsAl:
                if ucCol == ucAl:
                    r += 1
    if r >= 2 or ucsAl == set():
        return False
    else:
        return True

def horario(ucs,alunos):
    res = []
    col = colis(ucs)
    
    for aluno in alunos:
        flag = True
        cadeiras = alunos[aluno]
        if cols(col,cadeiras):
            n = 0
            for uc in cadeiras:
                if uc in ucs:
                    n += ucs[uc][2]
                else:
                    flag = False
                    continue
            if flag:
                res.append((aluno,n))
                
    res.sort(key = lambda x:(-x[1],x[0]))
    return res