import ply.lex as lex

code = """
def soma_quadrados(a, b):
    if a > 0 and b > 0:
        res = a * a + b * b
    else:
        res = 0
    return res
    
def outra_funcao(x):
    return x * 2
    
    ## Comentário

print(soma_quadrados(3, 4))
"""

reserved = {
    'def': 'DEF',
    'if': 'IF',
    'else': 'ELSE',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT',
    'return': 'RETURN',
    'True': 'TRUE',
    'False': 'FALSE',
    'for': 'FOR',
    'while': 'WHILE',
    'break': 'BREAK',
    'continue': 'CONTINUE',
    'print': 'PRINT'
}

tokens = ['NUMBER', 'FUNC', 'VAR'] + list(reserved.values())

literals = ['(', ')', ':', '=', '*', '+', '-', '/', '<', '>', '[', ']', '{', '}', ',', '.', ';']

t_NUMBER = r'\d+'

def t_FUNC(t):
    r'[A-Za-z_]+(?=\()'
    t.type = reserved.get(t.value, 'FUNC')
    return t

def t_VAR(t):
    r'[A-Za-z_]+'
    t.type = reserved.get(t.value, 'VAR')
    return t


t_ignore = ' \t\r\n'
t_ignore_COMMENT = r'\#.*'

def t_error(t):
    print(f'Illegal character: {t.value[0]}')
    t.lexer.skip(1)

anaLexer = lex.lex()

anaLexer.input(code)

for token in anaLexer:
    print(token)