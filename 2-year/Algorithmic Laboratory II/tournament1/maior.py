"""

Implemente uma função que dada uma string s só com digitos e um inteiro k, 
devolve a maior substring de s com k digitos cuja soma dos digitos é a maior possível. 
Em caso de empate, deve devolver a substring com o maior número.

"""

def maior(s,k):
    max = 0
    sub = []
    
    if s == '' or k <= 0:
        return ''
    
    if k >= len(s):
        return s
    
    for i in range(len(s)-k):
        soma = 0
        string = []
        for j in range(k):
            soma += int(s[i+j])
            string.append(s[i+j])
            if max < soma:
                max = soma
                sub = "".join(string)
                
            elif max == soma:
                max1 = int(sub)
                max2 = int("".join(string))
            
                if max2 > max1:
                    sub = "".join(string)
        
    return sub