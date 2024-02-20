'''

Pretende-se que implemente uma função que detecte códigos ISBN inválidos. 
Um código ISBN é constituído por 13 digitos, sendo o último um digito de controlo.
Este digito de controlo é escolhido de tal forma que a soma de todos os digitos, 
cada um multiplicado por um peso que é alternadamente 1 ou 3, seja um múltiplo de 10.
A função recebe um dicionário que associa livros a ISBNs,
e deverá devolver a lista ordenada de todos os livros com ISBNs inválidos.

'''

def isbn(livros):
    final = []
    
    for x,y in livros.items():
        str_lista = list(y)
        num_lista = [int(j) for j in str_lista]
        
        mult_um = [a for a in num_lista [0::2]]
        mult_tres = [b*3 for b in num_lista [1::2]]
        
        soma = sum(mult_um) + sum(mult_tres)
        
        if soma%10 != 0:
            final.append(x)
            
    final.sort()        
    
    return final