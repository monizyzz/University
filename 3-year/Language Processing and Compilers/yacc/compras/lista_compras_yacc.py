#
# Listas de Compras: calculador do valor gasto
#

import sys
import ply.yacc as yacc

from lista_compras_lex import tokens


def p_lista(p):
    "lista : seccoes"
    print("Total de items: ",parser.items)
    print("Total de artigos comprados: ",parser.artigos)
    print("Valor total das compras: ",p[1])


def p_seccoes_seccao(p):
    "seccoes : seccoes seccao"
    p[0] = p[1] + p[2]


def p_seccoes_empty(p):
    "seccoes : "
    p[0] = 0


def p_seccao(p):
    "seccao : ID ':' produtos"
    p[0] = p[3] 

def p_produtos_produto(p):
    "produtos : produto"
    p[0] =p[1]
    
def p_produtos_produtos(p):
    "produtos : produtos produto"
    p[0] =p[1] + p[2]
  
def p_produto(p):
    "produto : '-' cod SEP prod SEP FLOAT SEP INT ';'"
    parser.items += 1
    parser.artigos += p[8]
    p[0] = p[6] * p[8]

def p_cod(p):
    "cod : INT"
def p_prod(p):
    "prod : ID"
    
def p_error(p):
    print('Syntax error!', p)
    parser.sucesso = False
    
print("inicio do Parser e do Processamento")
parser = yacc.yacc(debug=True)

parser.sucesso = True
parser.items = 0
parser.artigos = 0

content=""
for linha in sys.stdin:
    content += linha
    
parser.parse(content)

if (parser.sucesso):
   print("Lista de Compras bem escrita e Reconhecida")
