# Importações
import ply.yacc as yacc
import ply.lex as lex

# Definição dos tokens
tokens = (
    'NUM',
    'DOT',
    'SEMI_COLON',
    'OPEN_BRACES',
    'CLOSE_BRACES'
)

t_DOT = r'\.'
t_SEMI_COLON = r';'
t_OPEN_BRACES = r'\['
t_CLOSE_BRACES = r'\]'

t_ignore = ' \n\t'  


def t_NUM(t):
    r'\d+\.?\d*'
    t.value = float(t.value)  
    return t

def t_error(t):
    print('Illegal character: ' + t.value[0])
    t.lexer.skip(1)

# Criação do analisador léxico
lexer = lex.lex()

# Regras de análise sintática
def p_S(p):
    "S : IS DOT"
    print("Valor total de intervalos: ", p[1]) 

def p_IS(p):
    "IS : I RI"
    p[0] = p[1] + p[2]

def p_RI_empty(p):
    "RI : "
    p[0] = 0 

def p_RI_non_empty(p):
    "RI : I RI"
    p[0] = p[1] + p[2]

def p_I(p):
    "I : OPEN_BRACES NUM SEMI_COLON NUM CLOSE_BRACES"
    p[0] = 1 # Cada intervalo contribui com 1 para o total

def p_error(p):
    print('Syntax error!', p)
    parser.sucesso = False

parser = yacc.yacc()
parser.sucesso = True

print("Início do Parser e do Processamento")
with open('testeDataSet.txt') as f:
    content = f.read()

parser.parse(content)

if parser.sucesso:
    print("Fim do processamento. Sucesso.")
