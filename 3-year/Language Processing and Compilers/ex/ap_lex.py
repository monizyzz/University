#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p "python3.withPackages(ps: [ ps.ply ])"
import ply.lex as lex
import sys

tokens = (
    'ID',
    'NUM',

    # Figuras geometricas
    'RECT',
    'CIRC',
    'QUAD',
    'TRIA',
    'LOSA',

    # Operações
    'AREA',
    'PERIMETRO')

t_ID = r'[A-Za-z_][\w_]*'
t_NUM = r'-?\d*\.?\d+'


def t_RECT(t):
    r'rect'
    return t


def t_CIRC(t):
    r'circ'
    return t


def t_QUAD(t):
    r'quad'
    return t


def t_TRIA(t):
    r'tria'
    return t


def t_LOSA(t):
    r'losa'
    return t


def t_AREA(t):
    r'area'
    return t


def t_PERIMETRO(t):
    r'perimetro'
    return t


def t_error(t):
    t.type = t.value[0]
    t.value = t.value[0]
    t.lexer.skip(1)
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


t_ignore = ' \t'

lexer = lex.lex()
if __name__ == '__main__':
    if (len(sys.argv) > 1):
        f = open(sys.argv[1])
        lexer.input(f.read())
        f.close()
        while tok := lexer.token():
            print(tok.type, end=" ")
        print()
        exit()

    while True:
        lines = []
        print("Enter lines of text (press Ctrl+D to finish):")

        try:
            while True:
                line = input()
                lines.append(line)
        except EOFError:
            pass

        if not lines:
            break

        source = '\n'.join(lines)
        lexer.input(source)
        while tok := lexer.token():
            print(tok.type, end=" ")
        print()
