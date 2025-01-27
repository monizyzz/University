#
#  REconhecedor Léxico para BibTeX
#

import ply.lex as lex

states = [
    ('VALOR', 'exclusive')
]

literals = [',','{','}']

tokens = (
    'TIPOreg',
    'CAMPO',
    'TEXTO',
    'IGUAL'
)

t_TIPOreg = r'(?i:@article|@book|@incollection|@inproceedings|@techreport|@misc)'
t_CAMPO   = r'[a-zA-Z]+'

def t_IGUAL(t):
    r'='
    t.lexer.begin('VALOR')      #neste momento muda de estado para aceitar o vaor
    return t
    
def t_VALOR_TEXTO(t):
    r'\"[^"]*\"|\{[^}]*\}'
    t.lexer.begin('INITIAL')    #no fim retorna ao estado inicial
    t.value = t.value[1:-1]
    return t
    
t_ANY_ignore = ' \n\t'

def t_ANY_error(t):
    print('Illegal character: ', t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

#lexer = lex.lex(reflags=re.UNICODE) 
#lexer = lex.lex(reflags=re.IGNORECASE) 

# Reading input
source = ""
f = open("bibtex.txt",encoding="utf-8")
#for linha in f:
#	source += linha
source = f.read()

lexer.input(source)
tok = lexer.token()
while tok:
        if (tok.type=='TIPOreg'):
           print("-------------------")
        print(tok)
        tok = lexer.token()
