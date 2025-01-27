# ------------------------------------------------------------
# calclex.py
#
# tokenizer for a simple expression evaluator for:
#   1. numbers
#   2. operations: +,-,*,/
#   3. grouping: ( ) 
#   4. registers: a..z
#   5. read: ?
#   6. print !
# 
# ------------------------------------------------------------
import ply.lex as lex
import sys

# List of token names.   This is always required
tokens = (
    'number', 'id', 'DUMP', 'and', 'or', 'not', 'le', 'ge'
)
# Literals
literals = ['+', '-', '*', '/', '(', ')', '?', '!','=','>','<']

t_DUMP = r'!!'
t_le = r'<='
t_ge = r'>='
 
# A regular expression rule with some action code
def t_number(t):
    r'\d+'
    t.value = int(t.value)    
    return t

def t_and(t):
    r'(?i:and)'
    return t
def t_or(t):
    r'(?i:or)'
    return t
def t_not(t):
    r'(?i:not)'
    return t
    
def t_id(t):
    r'[a-zA-Z]+'
    return t
 
#----------------------------------------
# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
 
# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'
 
# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

#----------------------------------------
# Build the lexer
lexer = lex.lex()

# Reading input
#for linha in sys.stdin:
#    lexer.input(linha) 
#    for tok in lexer:
#        print(tok)
        
