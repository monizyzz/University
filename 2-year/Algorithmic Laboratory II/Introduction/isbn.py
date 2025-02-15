'''

Pretende-se que implemente uma função que detecte códigos ISBN inválidos. 
Um código ISBN é constituído por 13 digitos, sendo o último um digito de controlo.
Este digito de controlo é escolhido de tal forma que a soma de todos os digitos, 
cada um multiplicado por um peso que é alternadamente 1 ou 3, seja um múltiplo de 10.
A função recebe um dicionário que associa livros a ISBNs,
e deverá devolver a lista ordenada de todos os livros com ISBNs inválidos.

'''

# 100%
def isbn(livros):
    res = []
    for livro in livros:
        n = 0
        tres = False
        for num in livros[livro]:
            if tres:
                n += 3*int(num)
            else:
                n += int(num)
            tres = not tres
            
        #print("livros[livro][-1]:",livros[livro][-1], " | ",livros[livro][-1],"n:", n,  " | ","n % 10:", n % 10)
        if (n % 10) != 0:
            res.append(livro)
    res.sort()    
    return res