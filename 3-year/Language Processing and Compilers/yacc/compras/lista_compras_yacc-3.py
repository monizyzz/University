#
# Lista de Compras: calculador e Validado Semântico
#
import sys
import ply.yacc as yacc

from lista_compras_lex import tokens


def p_lista(p):
    "lista : seccoes"
    p[0] = p[1]


def p_seccoes_seccao(p):
    "seccoes : seccoes seccao"
    p[0] = p[1] + p[2]


def p_seccoes_empty(p):
    "seccoes : "
    p[0] = 0


def p_seccao(p):
    "seccao : idsec ':' produtos"
    p[0] = p[3] 
    
def p_idsec(p):
    "idsec : ID "
    # adicionar seccao ao conjunto de seccoes
    if p[1] in parser.seccoes:
        parser.sucesso = False
        print('Erro! Secções repetidas: ', p[1])
    else:
        parser.seccoes.add(p[1])


def p_produtos_produto(p):
    "produtos : produto"
    p[0] =p[1]
    
def p_produtos_produtos(p):
    "produtos : produtos produto"
    p[0] =p[1] + p[2]
  
def p_produto(p):
    "produto : '-' INT SEP ID SEP FLOAT SEP INT ';'"
    p[0] = p[6] * p[8]
    info_produto = {
        'name': p[4],
        'price': p[6],
    }
    if p[2] in parser.produtos:
        parser.sucesso = False
        if info_produto == parser.produtos[p[2]]:
            print('Aviso! Múltiplas entradas do mesmo produto: ', p[2])
        else:
            print('Erro! Produtos diferentes com o mesmo ID: ', p[2])
    else:
        parser.produtos[p[2]] = info_produto

def p_error(p):
    print('Syntax error! ',p)
    parser.sucesso = False
    
#inicio do Parser e do Processamento
parser = yacc.yacc()

parser.sucesso = True
parser.seccoes = set()
parser.produtos = dict()

content=""
for linha in sys.stdin:
    content += linha
res = parser.parse(content)
if (parser.sucesso):
   print("Lista de Compras Correta! Frase Reconhecida e Processada!")
   print("Valor da despesa total calculado: %8.2f" % res)
else:
   print("Erros Sintáticos ou Semânticos não permitiram Reconher e Processar a frase dada")
