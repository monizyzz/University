#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p "python3.withPackages(ps: [ ps.ply ])"
import ply.yacc as yacc
from ap_lex import tokens
import sys
import math


def p_grammar(p):
    '''
    Programa   : Frase
               | Frase Programa
    
    Frase      : Declaracao
               | Calculo
    '''


def p_decl(p):
    '''
    Declaracao : Lista ':' Figura
    '''
    for id in p[1]:
        if id in parser.figures:
            print(f"\033[91mError: Figure '{id}' already defined\033[0m")
            parser.exito = False
            return
        parser.figures[id] = p[3]


def p_fig(p):
    '''
    Figura     : Rectangulo
               | Circulo
               | Quadrado
               | Triangulo
               | Losango
    '''
    p[0] = p[1]


def p_fig_args(p):
    '''
    Rectangulo : RECT '(' NUM ',' NUM ')'
    Circulo    : CIRC '(' NUM ')'
    Quadrado   : QUAD '(' NUM ')'
    Triangulo  : TRIA '(' NUM ',' NUM ')'
    Losango    : LOSA '(' NUM ',' NUM ')'
    '''
    type = p[1]
    args = []
    for i in range(3, len(p), 2):
        args.append(float(p[i]))
    p[0] = (type, args)


def p_calc(p):
    '''
    Calculo    : AREA '(' Lista ')'
               | PERIMETRO '(' ID ')'
    '''
    if p[1] == 'area':
        total = 0
        for id in p[3]:
            if id not in parser.figures:
                print(f"\033[91mError: Figure '{id}' not defined\033[0m")
                parser.exito = False
                return
            total += calculate_area(parser.figures[id])
        ids = ", ".join(p[3])
        print(f"Area of figures '{ids}' is {total:.2f}")
    else:
        id = p[3]
        if id not in parser.figures:
            print(f"\033[91mError: Figure '{id}' not defined\033[0m")
            parser.exito = False
            return
        print(
            f"Perimeter of figure '{id}' is {calculate_perimeter(parser.figures[id]):.2f}"
        )


def p_list(p):
    '''
    Lista      : ID
               | ID ',' Lista
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]


def p_error(p):
    if not p:
        print("\033[91mSyntax error: Unexpected end of input\033[0m")
        return

    # Extract line and column information
    lineno = p.lineno
    column = find_column(p)

    # Fetch the line content from the lexer
    line = p.lexer.lexdata.splitlines()[lineno - 1]

    # Get the expected tokens
    expected = []
    if hasattr(parser, 'state'):
        # Get state number
        state = parser.state
        # Get expected tokens for this state
        if state < len(parser.action):
            for token, action in parser.action[state].items():
                if token != '$end':
                    expected.append(token)

    # Format the expected tokens message
    expected_msg = f"Expected one of: {', '.join(repr(t) for t in expected)}" if expected else "Unexpected token"

    # Print the error message with red color
    print(f"\033[91mSyntax error at line {lineno}, column {column}:\033[0m")
    print(line)

    # Print an underline at the error position, in red
    underline = " " * (column - 1) + "\033[91m^\033[0m"
    print(underline)

    print(f"Found unexpected token '{p.value}' of type '{p.type}'")
    print(expected_msg)
    p.exito = False


def find_column(token):
    # Calculate the column position of the token in the line
    last_newline = token.lexer.lexdata.rfind('\n', 0, token.lexpos)
    if last_newline < 0:
        last_newline = -1
    return token.lexpos - last_newline


def calculate_area(figure):
    if figure[0] == 'rect':
        return figure[1][0] * figure[1][1]
    elif figure[0] == 'circ':
        return math.pi * figure[1][0]**2
    elif figure[0] == 'quad':
        return figure[1][0]**2
    elif figure[0] == 'tria':
        return (figure[1][0] * figure[1][1]) / 2
    elif figure[0] == 'losa':
        return (figure[1][0] * figure[1][1]) / 2
    return 0


def calculate_perimeter(figure):
    if figure[0] == 'rect':
        return 2 * (figure[1][0] + figure[1][1])
    elif figure[0] == 'circ':
        return 2 * math.pi * figure[1][0]
    elif figure[0] == 'quad':
        return 4 * figure[1][0]
    elif figure[0] == 'tria':
        base, height = figure[1]
        side = math.sqrt((base / 2)**2 + height**2)
        return base + 2 * side
    elif figure[0] == 'losa':
        d1, d2 = figure[1]
        side = math.sqrt((d1 / 2)**2 + (d2 / 2)**2)
        return 4 * side
    return 0


if __name__ == '__main__':
    parser = yacc.yacc()
    parser.exito = True
    parser.figures = {}

    if (len(sys.argv) > 1):
        f = open(sys.argv[1])
        parser.parse(f.read(), tracking=True)
        f.close()
        if parser.exito:
            print("\033[92mParsing completed successfully!\033[0m")
        exit()

    while True:
        parser.exito = True

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
        parser.parse(source, tracking=True)
        if parser.exito:
            print("\033[92mParsing completed successfully!\033[0m")
