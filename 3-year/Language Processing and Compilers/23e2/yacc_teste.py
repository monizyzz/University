import ply.yacc as yacc

from lex_teste import tokens

def p_Colecao(p): 
    "Colecao : Tuples '.'"
    p[0] = p[1]
    print("Lista de facetas:", p[0])
    print("Número total de tuplos reconhecidos", parser.tot)
    
def p_Tuples1(p):
    "Tuples : Tuple"
    p[0] = p[1]
    
def p_Tuples2(p):
    "Tuples : Tuples Tuple"
    p[0] = p[1] + p[2]
    
def p_Tuple(p):
    "Tuple : TId '(' Faces ')'"
    parser.tot += 1
    p[0] = p[3]
    
def p_Faces1(p):
    "Faces : Faceta"
    p[0] = [p[1]]
    
def p_Faces2(p):
    "Faces : Faces ',' Faceta"
    p[0] = p[1] + [p[3]] 
    
def p_Faceta(p):
    "Faceta : Field '=' Value"
    p[0] = p[1]
    
def p_Field(p):
    "Field : PAL"
    p[0] = p[1]
    
def p_Value(p):
    "Value : STR"

def p_TId(p):
    "TId : PAL"
    
def p_error(p):
    print('Syntax error!', p)
    
parser = yacc.yacc()

parser.tot = 0

with open('data.txt') as f:
    content = f.read()

parser.parse(content)