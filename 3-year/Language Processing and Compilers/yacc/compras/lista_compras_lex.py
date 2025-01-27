from sys import *
import ply.lex as lex

tokens = ('SECCAO', 'ID', 'INT', 'FLOAT', 'SEP')

literals = ['-', ',', ':', ';']


t_SEP = r'::'

def t_SECCAO(t):
    r'(?i:talho|peixaria|frutaria)'
    return t
    
def t_ID(t):
    r'[a-zA-Z]+'
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

t_ignore = ' \n\t'

def t_error(t):
    print('Illegal character: ' + t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()


#with open('exemplos/exemplo1.txt') as f:
#    content = f.read()
#lexer.inpu(content)
#
for linha in stdin:
    lexer.input(linha)
    for token in lexer:
        print(token)
