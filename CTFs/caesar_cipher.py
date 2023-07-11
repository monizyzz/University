frase = "LEBEFNE HLZK LEUVI IZSSFE"

for i in range (1, 26):
    for letter in frase:
        if letter != ' ':
            x = ord(letter)
            x += i
            if x > 90 : 
                x -= 26
            x = chr(x)
            print(x, end='')
        else : 
            print(' ', end= '')    
    print('\n')    
    
result = "UNKNOWN QUIT UNDER RIBBON"