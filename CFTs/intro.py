def soma (a,b):
    print (a + b)
    
def subtração (a,b):
    print (a - b)
    
def mult (a,b):
    print (a * b)
    
def div (a,b):
    print (a / b)


def menu ():
    calculadora = input ("Calculadora Machibomba\n")
    op = input ("Operação: \n1 - Soma\n2 - Subtração\n3 - Multiplicação\n4 - Divisão\n")
    a = int(input ("Indique o primeiro número: "))
    b = int(input ("Indique o segundo número: "))
    
    if int(op) == 1:
        soma(a,b)
        
    if int(op) == 2:
        subtração(a,b)
        
    if int(op) == 3: 
        mult(a,b)
    
    if int(op) == 4:
        div(a,b)
        
menu()