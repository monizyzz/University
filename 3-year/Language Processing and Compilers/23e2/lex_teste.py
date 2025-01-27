import ply.lex as lex

tokens = ('PAL', 'STR')
literals = ['(', ')', ',', '=', '.']

def t_STR(t):
    r'\"[^\"]*\"'
    return t

def t_PAL(t):
    r'[a-zA-Z]+'
    return t

    
t_ignore = ' \n\t'

def t_error(t):
    print('Illegal character: ' + t.value[0])
    t.lexer.skip(1)
    


lexer = lex.lex() # cria um AnaLex especifico a partir da especificação acima usando o gerador 'lex' do objeto 'lex'


with open('data.txt') as f:
    lexer.input(f.read())
    for tok in lexer:
        print(tok)
        