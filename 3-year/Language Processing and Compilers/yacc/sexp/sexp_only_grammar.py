

#12
#qqqq
#(1 2 3 4)
#(add (mul a b)(div 14 7))
#(a mul b) add (14 div 7)
#a b mul 14 7 div add
#()
#(  qqq )
#(546 )
#( if (maior a 5)(thenblock)(elseblock) )
#
# 67 888
#eee iiii lll
#((yyy)
 
import ply.yacc as yacc
import sys

from sexp_lex import tokens

## inicio da GIC

def p_lisp_grammar(p):
    """
       lisp : sexp
       sexp : PAL
       sexp : NUM
       sexp : LPAREN sexplist RPAREN
       sexplist : sexp sexplist
       sexplist :   sexp  
    """
# T = { PAL, NUM, LPAREN, RPAREN }
def p_error(p):
    parser.success = False
    print('Syntax error!', p)

###inicio do parsing
parser = yacc.yacc()

for linha in sys.stdin:
    parser.success = True
    parser.parse(linha)
    if parser.success:
       print('Parsing successful!')
   
