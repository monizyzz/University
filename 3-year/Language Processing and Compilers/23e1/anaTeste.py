import ply.lex as lex

tokens = ( 
            'NUM',
            'OPEN_BRACES',
            'CLOSE_BRACES',
            'DOT',
            'SEMI_COLON',
        )

t_DOT = r'\.'
t_ignore = ' \n\t' 

def t_SEMI_COLON(t):
    r';'
    return t

def t_NUM(t):
    r'\d+\,\d+'
    return t

def t_OPEN_BRACES(t):
    r'\['
    return t

def t_CLOSE_BRACES(t):
    r'\]'
    return t

def t_error(t):
    print('Illegal character: ' + t.value[0])
    t.lexer.skip(1)
    return

lexer = lex.lex()


with open('testeDataSet.txt') as f:
    content = f.read()

lexer.input(content)
for token in lexer:
    print(token)
