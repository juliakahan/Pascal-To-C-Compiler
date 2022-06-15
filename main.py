from ply import lex
from ply import yacc


reserved = {
    'and': 'AND',
    'array': 'ARRAY',
    'begin': 'BEGIN',
    'case': 'CASE',
    'const': 'CONST',
    'div': 'DIV',
    'do': 'DO',
    'downto': 'DOWNTO',
    'else': 'ELSE',
    'end': 'END',
    'file': 'FILE',
    'for': 'FOR',
    'function': 'FUNCTION',
    'goto': 'GOTO',
    'if': 'IF',
    'in': 'IN',
    'label': 'LABEL',
    'mod': 'MOD',
    'nil': 'NIL',
    'not': 'NOT',
    'of': 'OF',
    'or': 'OR',
    'procedure': 'PROCEDURE',
    'program': 'PROGRAM',
    'record': 'RECORD',
    'repeat': 'REPEAT',
    'set': 'SET',
    'then': 'THEN',
    'to': 'TO',
    'type': 'TYPE',
    'until': 'UNTIL',
    'var': 'VAR',
    'while': 'WHILE',
    'with': 'WITH',
    'eof': 'EOF',
    'eoln': 'EOLN',
    'false': 'FALSE',
    'true': 'TRUE',
    'input': 'INPUT',
    'output': 'OUTPUT',
    'get': 'GET',
    'put': 'PUT',
    'readln': 'READLN',
    'read': 'READ',
    'write': 'WRITE',
    'writeln': 'WRITELN',
    'lineend': 'LINEEND',
    'sqrt': 'SQRT',
    'text': 'TEXT',
    'dispose': 'DISPOSE',
    'integer': 'INTEGER',
    'char': 'CHAR',
    'real': 'REAL',
    'boolean': 'BOOLEAN',

}
tokens = [

    # literals
    'EMPTY', 'ID',

    # opertators
    'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'EQ', 'LT', 'GT', 'DOT',
    'COMA', 'REF', 'DOLLAR', 'HASH', 'BINLSO', 'BINRSO', 'ISDIFF', 'SYMMDIFF', 'LOREQ',
    'GOREQ', 'ASSIG', 'INCRBY', 'DECRBY', 'MULTBY', 'DIVBY',

    # delimeters
    'LP', 'RP', 'LBR', 'RBR', 'SEMICOL', 'COL', 'LCURLBR', 'RCURLBR', 'NEWLINE'

] + list(reserved.values())
#'LCOMM', 'RCOMM', 'LGROUP', 'RGROUP',
#, 'EXPON'
# Operators

t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_COMA = r','
t_DOT = r'.'
t_BINLSO = r'<<'
t_BINRSO = r'>>'
# t_EXPON = r'\**'
t_HASH = r'\#'
t_DOLLAR = r'\$'
t_LT = r'<'
t_GT = r'>'
t_LOREQ = r'<='
t_GOREQ = r'>='
t_EQ = r'='
t_ISDIFF = r'<>'
t_SYMMDIFF = r'><'
t_ASSIG = r':='
t_INCRBY = r'\+='
t_DECRBY = r'\-='
t_MULTBY = r'\*='
t_DIVBY = r'\\='
t_REF = r'@'

# delimeters
t_LP = r'\('
t_RP = r'\)'
t_LBR = r'\['
t_RBR = r'\]'
t_SEMICOL = r';'
t_COL = r':'
t_LCURLBR = r'\{'
t_RCURLBR = r'\}'
# t_LCOMM = r'\(*'
# t_RCOMM = r'\*)'
# t_LGROUP = r'\(.'
# t_RGROUP = r'\.)'

# ID
#t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
# INTEGER
t_INTEGER = r'^[1-9][0-9]*|0$'
# CHAR
t_CHAR= r'(L)?\'([^\\\n]|(\\.))*?\''
# REAL
t_REAL = r'((\+|-)?([0-9]+)(\.[0-9]+)?)|((\+|-)?\.?[0-9]+)'
# BOOLEAN
t_BOOLEAN = r'(true|false)'
t_EMPTY = r'""'
t_NEWLINE = r'\n+'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'VAR'  # Check for reserved words if not in reserved words than its VAR
    return t


# A string containing ignored characters (spaces and tabs)
t_ignore = ' '


# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()
with open('test1_correct_syntax') as f:
    lines = f.readlines()
content = "".join(lines)

lexer.input(content)


# lexer = lex.lex()
c_Code = ""

def p_empty(p):
    '''empty : '''
    p[0] = ""

def p_pascal_program(p):
    '''
    pascal_program : program_id LBR id_list RBR SEMICOL program_block DOT
    | program_id SEMICOL program_block DOT
    '''
    if(p.length == 7 ):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7]
    elif(p.length == 4):
        p[0] = p[1] + p[2] + p[3] + p[4]
    global c_Code
    c_Code = p[0]


def p_program_id(p):
    '''
    program_id : PROGRAM ID
     '''
    p[0] = p[1] + p[2]



def p_id_list(p):
    '''
    id_list : id_list COMA ID
    | ID
    '''
    if(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    elif(p.length == 1):
        p[0] = p[1]



#==================================================some helpful grammar definitions============================================



def p_opt_declarations(p):
    '''
    opt_declarations : declarations
        | empty
    '''
    p[0] = p[1]


def p_opt_subprogram_declarations(p):
    '''
    opt_subprogram_declarations : subprogram_declarations
    | EMPTY
    '''
    p[0] = p[1]


def p_opt_comp_statements(p):
    '''
    opt_comp_statements : comp_statement
    | EMPTY
    '''
    p[0] = p[1]


#======================================================================================================================================================

def p_program_block(p):
    '''

   program_block : opt_declarations opt_subprogram_declarations opt_comp_statements
    '''
    p[0] = p[1] + p[2] + p[3]



def p_declarations(p):
    '''
    declarations : declarations VAR NEWLINE id_list COL type
    | VAR NEWLINE id_list COL type
    '''
    if (p.length == 6):
        p[0] = p[1] + [2] + p[3] + p[4] + p[5] + p[6]
    elif (p.length == 5):
        p[0] = p[1] + p[2] + p[3] + p[4] +p[5]



def p_type(p):
    '''
    type : standard_type
    |  ARRAY LBR num DOT DOT num RBR OF standard_type
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 9):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8] + p[9]


def p_num(p):
    '''
    num : signed_integer
    | signed_integer DOT INTEGER
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]



def p_signed_integer(p):
    '''
    signed_integer : INTEGER
    | MINUS INTEGER
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 2):
        p[0] = p[1] + p[2]



def p_subprogram_declarations(p):
    '''
    subprogram_declarations : subprogram_declarations subprogram_declaration SEMICOL
    | subprogram_declaration
    '''
    if (p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    elif (p.length == 1):
        p[0] = p[1]


def p_subprogram_declaration(p):
    '''
    subprogram_declaration : subprogram_head declarations comp_statement
    | EMPTY
    '''
    if (p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    elif (p.length == 1):
        p[0] = p[1]


def p_subprogram_head(p):
    '''
    subprogram_head : function_id arguments COL standard_type SEMICOL
    | program_id arguments SEMICOL
    '''

    if(p.length == 5):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]



def p_standard_type(p):
    '''
    standard_type : INTEGER
    | REAL
    '''
    p[0] = p[1]



def p_sign(p):
    '''
    sign : PLUS
    | MINUS
    '''
    p[0] = p[1]


def p_logic_operator(p):
    '''
    logic_operator : OR
    | AND
    '''
    p[0] = p[1]


def p_logic_statement(p):
    '''
    logic_statement : comparison
    | BOOLEAN
    '''
    p[0] = p[1]


def p_function_id(p):
    '''
    function_id : ID
    '''
    p[0] = p[1]


def p_arguments(p):
    '''
    arguments : LP parameter_list RP
    '''
    p[0] = p[1] + [p[2]] + p[3]



def p_parameter_list(p):
    '''

    parameter_list : id_list COL type
    '''
    p[0] = [p[1]] + p[2] + p[3]



def p_comp_statement(p):
    '''
    comp_statement : BEGIN optional_statements END
    '''
    p[0] = p[1] + p[2] + p[3]


def p_optional_statements(p):
    '''
    optional_statements : statement_list
    '''
    p[0] = p[1]


def p_statement_list(p):
    '''

    statement_list : statement
    | statement_list SEMICOL statement
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]


def p_variable(p):
    '''
    variable : ID
    | ID LBR expression RBR
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 4):
        p[0] = p[1] + p[2] + p[3] +p[4]


def p_statement(p):
    '''
    statement : variable ASSIG expression
    | procedure_statement
    | comp_statement
    | IF expression THEN statement ELSE statement
    | WHILE expression DO statement
    '''
    if(p.length == 3):
        p[0] = p[1]


def p_procedure_statement(p):
    '''
    procedure_statement : ID
    | ID LP expression_list RP
    '''
    if(p.length == 1):
        p[0]= p[1]
    elif(p.length == 4):
        p[0] = p[1] + p[2] + [p[3]] + p[4]

def p_simple_expression(p):
    '''
    simple_expression : simple_expression PLUS term
    | sign term
    | term
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 2):
        p[0] = p[1] + p[2]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]

def p_expression(p):
    '''
    expression : simple_expression EQ simple_expression
    | simple_expression
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif (p.length == 3):
        p[0] = p[1] + p[2] + p[3]



def p_expression_list(p):
    '''
    expression_list : expression
    | expression_list COMA expression
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]


def p_to_expression(p):
    '''
    to_expression : TO
    | DOWNTO
    '''
    p[0]=p[1]


def p_for(p):
    '''
    for : FOR ID ASSIG expression to_expression expression
    '''
    p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6]





def p_term(p):
    '''
    term : factor
    | term MULTIPLY factor
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]



def p_factor(p):
    '''
    factor : ID
    | ID LP expression_list RP
    | num
    | LP expression RP
    | NOT factor
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 2):
        p[0] = p[1] + p[2]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    elif(p.lentgh == 4):
        p[0] = p[1] + p[2] + p[3] + p[4]



def p_comp_operator(p):
    '''
    comp_operator : LT
    | GT
    | LOREQ
    | GOREQ
    | EQ
    '''
    p[0] = p[1]


def p_comparison(p):
    '''
    comparison : num comp_operator num
    '''
    p[0] = p[1] + p[2] + p[3]


def p_statement_logic_operators(p):
    '''
    statement_logic_operators : logic_operator
    | AND THEN
    | OR ELSE
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 2):
        p[0] = p[1] + p[2]

def p_error(p):
    print("Syntax error at '%s'\n" % p.value)
    global wasError
    wasError = True

def p_logic_condition(p):
    '''
    logic_condition : NOT LP logic_statement statement_logic_operators logic_statement RP
    | LP logic_statement statement_logic_operators logic_statement RP
    '''
    if(p.length == 6):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6]
    elif(p.length == 5):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5]



with open('test1_correct_syntax') as f:
    lines = f.readlines()
code = "".join(lines)
parser = yacc.yacc()
parser.parse(code)
with open('test1_out.txt', 'w') as file:
    file.write(c_Code)


#(* Convert  Celsius temperature to Fahrenheit *)