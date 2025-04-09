"""

Um ladrão assalta uma casa e, dado que tem uma capacidade de carga limitada, 
tem que decidir que objectos vai levar por forma a maximizar o potencial lucro. 

Implemente uma função que ajude o ladrão a decidir o que levar.
A função recebe a capacidade de carga do ladrão (em Kg) seguida de uma lista 
dos objectos existentes na casa, sendo cada um um triplo com o nome, o valor de 
venda no mercado negro, e o seu peso. Deve devolver o máximo lucro que o ladrão
poderá  obter para a capacidade de carga especificada.

"""

# 70%
def calc(capacidade, lucro,objectos):
    if capacidade == 0:
        return lucro
    elif objectos == []:
        return lucro
    elif capacidade > 0:
        head = objectos[0]
        nome, cash, peso = head
        if peso <= capacidade:
            u1 = calc(capacidade-peso,lucro+cash,objectos[1:])
            u2 = calc(capacidade,lucro,objectos[1:])
            #print("u1:",u1,"| u2:",u2)
            if u1 and u2:
                if u2 < u1:
                    return u1
                else:
                    return u2
            elif u1 and not u2:
                return u1
            elif not u1 and u2:
                return u2
        else:
            calc(capacidade,lucro,objectos[1:])
    


def ladrao(capacidade,objectos):
    #r = [] # (lucro, lista) 
    if capacidade <= 0 or objectos == []:
        return 0
    m = -1
    for i,objecto in enumerate(objectos):
        r = calc(capacidade, 0,objectos[i:])
        if r > m:
            m = r
    return m