import ply.lex as lex

tokens = (
    'NUM',
    'PAL',
    'LPAREN',
    'RPAREN'
)

#def t_NUM(t):
#    r'('+'|'-')?\d+(\.\d+)?'
#    #t.value = float(t.value)
#    return t

t_NUM = r"('+'|'-')?\d+(\.\d+)?"
t_PAL = r'[a-zA-Z]\w*'

t_LPAREN = r'\('
t_RPAREN = r'\)'

t_ignore = ' \n\t'

def t_error(t):
    print('Illegal character: ', t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()
