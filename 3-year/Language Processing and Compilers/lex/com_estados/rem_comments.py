import ply.lex as lex
import sys

# Declare the state
states = (
    ('COMMENT', 'exclusive')
)

# List of token names.   
tokens = ('CBEGIN', 'CEND', 'CONTENT')

#--no estado INITIAL: 
def t_CBEGIN(t):
    r'\/\*'
    t.lexer.begin('COMMENT')
    #print('ENTREI NUM COMENTÁRIO')
def t_CONTENT(t):
    r'(.|\n)'
    print(t.value, end='')

#--no estado COMMENT: 
def t_COMMENT_CEND(t):
    r'\*\/'
    t.lexer.begin('INITIAL')
    #print('SAÍ DE UM COMENTÁRIO')

def t_COMMENT_CONTENT(t):
    r'(.|\n)'
    pass

#--em qualquer um dos 2 estados: 
def t_ANY_error(t):
    t.lexer.skip(1)
    #print('Illegal character: ' + t.value[0])

#~~~~definidas as regras, o processamento
lexer = lex.lex()
for line in sys.stdin:
    lexer.input(line)
    for tok in lexer:
        pass
