import ply.yacc as yacc
import sys

from sexp_lex import tokens

## incio da GIC

def p_lisp(p):
    "lisp : sexp"
    print(p[1])


def p_sexp_pal(p):
    "sexp : PAL"
    p[0] = "pushs \"" + p[1] + "\"\n"

def p_sexp_num(p):
    "sexp : NUM"
    p[0] = "pushi " + p[1] + "\n"

def p_sexp_sexplist(p):
    "sexp : LPAREN sexplist RPAREN"
    global count
    count += 1
    p[0] = f"BL{count}\n" + p[2] + f"EL{count}\n" 
    


def p_sexplist_sexp(p):
    "sexplist : sexp sexplist"
    p[0] = p[1] + p[2]


def p_sexplist_empty(p):
    "sexplist : "
    p[0] = ""


def p_error(p):
    parser.success = False
    print('Syntax error!')
    exit()

###inicio do parsing
parser = yacc.yacc()
parser.success = True

for linha in sys.stdin:
    count = 0
    parser.parse(linha)
    
