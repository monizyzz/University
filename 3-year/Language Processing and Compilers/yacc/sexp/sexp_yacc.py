#
# Parser para reconhecer e processar SExpressions
#
import sys
import ply.yacc as yacc
from sexp_lex import tokens

## incio da GIC

def p_lisp(p):
    "lisp : sexp"
    p[0] = p[1]
    print('Parsing completed succesfully! ', p[1])


def p_sexp_pal(p):
    "sexp : PAL"
    print('Reconheci um atomo ', p[1])
    p[0] = p[1]

def p_sexp_num(p):
    "sexp : NUM"
    print('Reconheci um numero ', p[1])
    p[0] = p[1]

def p_sexp_sexplist(p):
    "sexp : LPAREN sexplist RPAREN"
    print('Reconheci uma lista completa', p[2])
    p[0] = p[2]

def p_sexplist_sexp(p):
    "sexplist : sexplist sexp"
    print('Reconheci uma cabeça e uma cauda ')
    p[0] = p[1] + [p[2]]
    


def p_sexplist_empty(p):
    "sexplist : "
    print('Reconheci uma lista vazia ')
    p[0] = []


def p_error(p):
    analisadorSintatico.success = False
    print('Syntax error!')
    #exit()

###inicio do parsing
analisadorSintatico = yacc.yacc()
analisadorSintatico.success = True
    
for linha in sys.stdin:
    analisadorSintatico.parse(linha)
  


