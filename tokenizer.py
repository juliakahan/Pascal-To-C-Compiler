# from ply import lex
# from ply import yacc
#
# tokens = [
#
#     # reserved
#     'AND', 'ARRAY', 'BEGIN', 'CASE', 'CONST', 'DIV', 'DO', 'DOWNTO', 'ELSE', 'END', 'FILE', 'FOR', 'FUNCTION',
#     'GOTO', 'IF', 'IN', 'LABEL', 'MOD', 'NIL', 'NOT', 'OF', 'OR', 'PROCEDURE', 'PROGRAM', 'RECORD', 'REPEAT',
#     'SET', 'THEN', 'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE', 'WITH', 'EOF', 'EOLN', 'FALSE', 'TRUE', 'INPUT',
#     'OUTPUT', 'GET', 'PUT', 'READLN', 'READ', 'WRITE', 'WRITELN', 'LINEEND', 'SQRT', 'TEXT', 'DISPOSE',
#
#     # literals
#     'ID', 'INTEGER', 'CHAR', 'REAL', 'BOOLEAN',
#
#     # opertators
#     'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'EQ', 'LT', 'GT', 'DOT',
#     'COMA', 'REF', 'DOLLAR', 'HASH', 'BINLSO', 'BINRSO', 'EXPON', 'ISDIFF', 'SYMMDIFF', 'LOREQ',
#     'GOREQ', 'ASSIG', 'INCRBY', 'DECRBY', 'MULTBY', 'DIVBY',
#
#     # delimeters
#     'LP', 'RP', 'LBR', 'RBR', 'SEMICOL', 'COL', 'LCURLBR', 'RCURLBR', 'LCOMM', 'RCOMM', 'LGROUP', 'RGROUP',
#
# ]
#
# # Operators
# t_PLUS = r'\+'
# t_MINUS = r'-'
# t_MULTIPLY = r'\*'
# t_DIVIDE = r'/'
# t_COMA = r','
# t_DOT = r'.'
# t_BINLSO = r'<<'
# t_BINRSO = r'>>'
# t_EXPON = r'**'
# t_HASH = r'#'
# t_DOLLAR = r'$'
# t_LT = r'<'
# t_GT = r'>'
# t_LOREQ = r'<='
# t_GOREQ = r'>='
# t_EQ = r'='
# t_ISDIFF = r'<>'
# t_SYMMDIFF = r'><'
# t_ASSIG = r':='
# t_INCRBY = r'\+='
# t_DECRBY = r'\-='
# t_MULTBY = r'\*='
# t_DIVBY = r'\\='
# t_REF = r'@'
#
# # delimeters
# t_LP = r'\('
# t_RP = r'\)'
# t_LBR = r'\['
# t_RBR = r'\]'
# t_SEMICOL = r';'
# t_COL = r':'
# t_LCURLBR = r'\{'
# t_RCURLBR = r'\}'
# t_LCOMM = r'\(*'
# t_RCOMM = r'\*)'
# t_LGROUP = r'\(.'
# t_RGROUP = r'\.)'
#
#
# def t_newline(t):
#     r'\n+'
#     t.lexer.lineno += len(t.value)
#
#     # A string containing ignored characters (spaces and tabs)
#
# t_ignore = ' \t'
#
#
# # Error handling rule
# def t_error(t):
#     print("Illegal character '%s'" % t.value[0])
#     t.lexer.skip(1)
#
#
#
# lexer = lex.lex()
#
# # with open('test.txt') as f:
# #     lines = f.readlines()
# #
# # content = "".join(lines)
# #
# # lexer.input(content)