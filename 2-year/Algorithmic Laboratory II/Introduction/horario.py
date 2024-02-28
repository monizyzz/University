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

# 20%
def horario(ucs,alunos):
    final = []
    horas_alunos = {}
    
    for aluno, ucs_aluno in alunos.items():
        horas_alunos[aluno] = 0
        for uc in ucs_aluno:
            if uc in ucs:
                horas_alunos[aluno] += ucs[uc][2]
    
    final = [(alunos, horas) for aluno, horas in horas_alunos.items() if len(alunos[aluno].intersection(ucs.keys())) == len(alunos[aluno])]
    
    final.sort(key=lambda x: x[0])
    final.sort(key=lambda x: (x[1]), reverse=True)
    
    return final