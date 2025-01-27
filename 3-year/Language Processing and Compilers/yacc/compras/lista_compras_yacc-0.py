#
# Lista de Compras: analisador sintático puro
#

import sys
import ply.yacc as yacc

from lista_compras_lex import tokens

def p_lista_grammar(p):
    """
    lista : seccoes
    seccoes : seccoes seccao
    seccoes : 
    seccao : ID ':' produtos
    produtos : produto
             | produtos produto
    produto : '-' codP SEP nomeP SEP precU SEP qt ';'
    codP  :  INT
    nomeP :  ID
    precU :  FLOAT
    qt    :  INT 
    """

def p_error(p):
    print('Syntax error when reading ',p)
    parser.sucesso = False

#inicio do Parser e do Processamento
parser = yacc.yacc()

parser.sucesso = True

#with open('lista_compras_exe.txt') as f:
#f = open('lista_compras_exe.txt')
#content = f.read()

content=""
for linha in sys.stdin:
    content += linha

parser.parse(content)

if (parser.sucesso):
   print("Lista de Compras bem escrita e Reconhecida")
   
