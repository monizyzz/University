"""

Implemente uma função que formata um programa em C.
O código será fornecido numa única linha e deverá introduzir
um '\n' após cada ';', '{', ou '}' (com excepção da última linha).
No caso do '{' as instruções seguintes deverão também estar identadas
2 espaços para a direita.

"""

# 20%
def formata(codigo):
    codigo_formatado = ""
    indentadas = 0
        
    for char in codigo:
        if char == ';' and codigo.index(char) == len(codigo) - 1:
            codigo_formatado += char
            return codigo_formatado
            
        elif char == ';':
            codigo_formatado += char + '\n' + ' ' * indentadas
            
        elif char == '{':
            codigo_formatado += char + '\n' + ' ' * indentadas
            indentadas += 1
            
        elif char == '}' and codigo.index(char) == len(codigo) - 1:
            indentadas -= 1
            codigo_formatado += '\n' + ' ' * indentadas + char
            return codigo_formatado
            
        elif char == '}':
            indentadas -= 1
            codigo_formatado += '\n' + ' ' * indentadas + char
            
        elif char == ' ' and codigo[codigo.index(char) + 1] == ' ':
            continue
        
        else:
            codigo_formatado += char
                
    return codigo_formatado

# Teste
codigo = "int main() {int x;x=0;     x=x+1;}"
codigo_formatado = formata(codigo)
print(codigo_formatado)


""" 
def formata(codigo):
    codigo_formatado = ""
    indentadas = 0
        
    for char in codigo:
        if char == ';' and codigo[codigo.index(char) + 1] == '\0':
            codigo_formatado += char
            return codigo_formatado
            
        elif char == ';':
            codigo_formatado += char + '\n' + ' ' * indentadas
            
        elif char == '{':
            indentadas += 1
            codigo_formatado += char + '\n' + ' ' * indentadas
            
        elif char == '}' and codigo[codigo.index(char) + 1] == '\0':
            codigo_formatado += char
            return codigo_formatado
            
        elif char == '}':
            indentadas -= 1
            codigo_formatado += char + '\n' + ' ' * indentadas
            
        elif char == ' ' and codigo[codigo.index(char) + 1] == ' ':
            continue
        
        else:
            codigo_formatado += char
                
        return codigo_formatado
    
"""