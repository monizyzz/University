"""

Implemente uma função que dado um dicionário com as preferências dos alunos
por projectos (para cada aluno uma lista de identificadores de projecto, 
por ordem de preferência), aloca esses alunos aos projectos. A alocação
é feita por ordem de número de aluno, e cada projecto só pode ser feito
por um aluno. A função deve devolver a lista com os alunos que não ficaram
alocados a nenhum projecto, ordenada por ordem de número de aluno.

"""

# 100%
def func(projetos,proj): # dá um projeto ao aluno
    for p in projetos:
        #print(p)
        if p in proj:
            continue
        else:
            return p
                
    return -1            
    

def aloca(prefs):
    alunos = []
    al_sem_proj = []
    proj_usados = []
    
    for i in prefs:
        alunos.append(i)
        al_sem_proj.append(i)
    
    alunos.sort()
    al_sem_proj.sort()


    for al in alunos:
        r = func(prefs[al],proj_usados)
        if r != -1:
            proj_usados.append(r)
            al_sem_proj.remove(al)
    
    
    return al_sem_proj

# or

def aloca(prefs):
    dic = {}
    alunos_nao_alocados = []
    
    for aluno in sorted(prefs):
        for projeto in prefs[aluno]:
            if projeto not in dic:
                dic[projeto] = aluno
                break
            
        else:
            alunos_nao_alocados.append(aluno)
    
    return alunos_nao_alocados