import ply.lex as lex
import sys

states = [
    ('COMMENT', 'exclusive')
]

tokens = ('CBEGIN', 'CEND',  'CONTENT', 'INLINE')

#--em qualquer um dos 2 estados
def t_ANY_CBEGIN(t):
    r'\/\*'
    t.lexer.push_state('COMMENT')
    #print('ENTREI NUM COMENTÁRIO')

def t_ANY_error(t):
    print('Illegal character: ' + t.value)

#--no estado COMMENT
def t_COMMENT_CEND(t):
    r'\*+\/'
    t.lexer.pop_state()
    #print('SAÍ DE UM COMENTÁRIO')

def t_COMMENT_CONTENT(t):
    r'(.|\n)'
    pass

#--no estado INITIAL
def t_INLINE(t):
    r'\/\/.*'

def t_CONTENT(t):
    r'(.|\n)'
    print(t.value, end='')

#~~~~definidas as regras, o processamento
lexer = lex.lex()
for line in sys.stdin:
    lexer.input(line)
    for tok in lexer:
        pass

