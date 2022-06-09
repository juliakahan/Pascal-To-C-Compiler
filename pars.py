from ply import lex
from ply import yacc

tokens = [

    # reserved
    'AND', 'ARRAY', 'BEGIN', 'CASE', 'CONST', 'DIV', 'DO', 'DOWNTO', 'ELSE', 'END', 'FILE', 'FOR', 'FUNCTION',
    'GOTO', 'IF', 'IN', 'LABEL', 'MOD', 'NIL', 'NOT', 'OF', 'OR', 'PROCEDURE', 'PROGRAM', 'RECORD', 'REPEAT',
    'SET', 'THEN', 'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE', 'WITH', 'EOF', 'EOLN', 'FALSE', 'TRUE', 'INPUT',
    'OUTPUT', 'GET', 'PUT', 'READLN', 'READ', 'WRITE', 'WRITELN', 'LINEEND', 'SQRT', 'TEXT', 'DISPOSE',

    # literals
    'ID', 'INTEGER', 'CHAR', 'REAL', 'BOOLEAN',

    # opertators
    'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'EQ', 'LT', 'GT', 'DOT',
    'COMA', 'REF', 'DOLLAR', 'HASH', 'BINLSO', 'BINRSO', 'EXPON', 'ISDIFF', 'SYMMDIFF', 'LOREQ',
    'GOREQ', 'ASSIG', 'INCRBY', 'DECRBY', 'MULTBY', 'DIVBY',

    # delimeters
    'LP', 'RP', 'LBR', 'RBR', 'SEMICOL', 'COL', 'LCURLBR', 'RCURLBR', 'LCOMM', 'RCOMM', 'LGROUP', 'RGROUP',

]

# Operators
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_COMA = r','
t_DOT = r'.'
t_BINLSO = r'<<'
t_BINRSO = r'>>'
t_EXPON = r'**'
t_HASH = r'#'
t_DOLLAR = r'$'
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
t_LCOMM = r'\(*'
t_RCOMM = r'\*)'
t_LGROUP = r'\(.'
t_RGROUP = r'\.)'

# ID
t_ID = r'[_a-zA-Z][_a-zA-Z0-9]+'
# INTEGER
t_INTEGER = r'^[1-9][0-9]*|0$'
# CHAR
t_CHAR = r'(L)?\'([^\\\n]|(\\.))*?\''
# REAL
t_REAL = r'((\+|-)?([0-9]+)(\.[0-9]+)?)|((\+|-)?\.?[0-9]+)'
# BOOLEAN
t_BOOLEAN = r'(true|false)'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

    # A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()


def p_pascal_program(p):
    '''
    program_id LBR id_list RBR SEMICOL program_block DOT
    | program_id SEMICOL program_block DOT
    '''
    if(p.length == 7 ):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7]
    elif(p.length == 4):
        p[0] = p[1] + p[2] + p[3] + p[4]



def p_program_id(p):
    '''
    program_id : PROGRAM ID
     '''
    p[0] = p[1] + p[2]
    return p[0]


def p_id_list(p):
    '''
    id_list : id_list COMA ID
    | ID
    '''
    if(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    elif(p.length == 1):
        p[0] = p[1]
    return p[0]


#==================================================some helpful grammar definitions============================================

def p_empty(p):
    '''
  empty : " "
    '''
    p[0] = p[1]
    return p[0]



def p_opt_declarations(p):
    '''
    opt_declarations : declarations
    | empty
    '''
    p[0] = p[1]
    return p[0]

def p_opt_subprogram_declarations(p):
    '''
    opt_subprogram_declarations : subprogram_declarations
    | empty
    '''
    p[0] = p[1]
    return p[0]

def p_opt_comp_statements(p):
    '''
    opt_comp_statements : comp_statements
    | empty
    '''
    p[0] = p[1]
    return p[0]

#======================================================================================================================================================

def p_program_block(p):
    '''

   program_block : opt_declarations opt_subprogram_declarations opt_comp_statement
    '''
    p[0] = p[1] + p[2] + p[3]
    return p[0]


def p_declarations(p):
    '''
    declarations : declarations VAR id_list COL type
    | VAR id_list COL type
    '''
    if (p.length == 5):
        p[0] = p[1] + p[2] + p[3] +p[4] + p[5]
    elif (p.length == 4):
        p[0] = p[1] + p[2] + p[3] +p[4]
    return p[0]


def p_type(p):
    '''
    type : standard_type
    |  ARRAY LBR num DOT DOT Num RBR OF standard_type
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 9):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6] + p[7] + p[8] + p[9]
    return p[0]

def p_num(p):
    '''
    num : signed_integer |
    signed_integer DOT INT
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    return p[0]


def p_signed_integer(p):
    '''
    signed_integer : INTEGER |
    MINUS INTEGER
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 2):
        p[0] = p[1] + p[2]
    return p[0]


def p_subprogram_declarations(p):
    '''
    subprogram_declarations : subprogram_declarations subprogram_declaration SEMICOL
    | subprogram_declaration
    '''
    if (p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    elif (p.length == 1):
        p[0] = p[1]
    return p[0]

def p_subprogram_declaration(p):
    '''
    subprogram_declaration : subprogram_head declarations compound_statement
    | empty
    '''
    if (p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    elif (p.length == 1):
        p[0] = p[1]
    return p[0]

def p_subprogram_head(p):
    '''
    subprogram_head : function_id arguments COL standard_type SEMICOL
    | procedure_id arguments SEMICOL
    '''

    if(p.length == 5):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    return p[0]


def p_standard_type(p):
    '''
    standard_type: INT
    | REAL
    '''
    p[0] = p[1]
    return p[0]


def p_sign(p):
    '''
    sign : PLUS
    | MINUS
    '''
    p[0] = p[1]
    return p[0]

def p_logic_operator(p):
    '''
    logic_operator : OR
    | AND
    '''
    p[0] = p[1]
    return p[0]

def p_logic_statement(p):
    '''
    logic_statement : comparison
    | BOOLEAN
    '''
    p[0] = p[1]
    return p[0]

def p_function_id(p):
    '''
    function_id : ID
    '''
    p[0] = p[1]
    return p[0]

def p_arguments(p):
    '''
    arguments: LP parameter_list RP
    '''
    p[0] = p[1] + [p[2]] + p[3]
    return p[0]


def p_parameter_list(p):
    '''

    parameter_list : id_list COL type
    '''
    p[0] = [p[1]] + p[2] + p[3]
    return p[0]


def p_comp_statement(p):
    '''
    comp_statement : BEGIN optional_statements END
    '''
    p[0] = p[1] + p[2] + p[3]
    return p[0]

def p_optional_statements(p):
    '''
    optional_statements : statements_list
    '''
    p[0] = p[1]
    return p[0]

def p_statement_list(p):
    '''

    statement_list : statement
    | statement_list SEMICOL statement
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    return p[0]

def p_variable(p):
    '''
    variable : ID
    | ID LBR expression RBR
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 4):
        p[0] = p[1] + p[2] + p[3] +p[4]
    return p[0]

def p_statement(p):
    '''
    statement : variable ASSIG expression
    | procedure_statement
    | compound_statement
    | IF expression THEN statement ELSE statement
    | WHILE expression DO statement
    '''
    if(p.length == 3):
        p[0] = p[1]
    return p[0]

def p_procedure_statement(p):
    '''
    procedure_statement : ID
    | ID LP expression_list RP
    '''
    if(p.length == 1):
        p[0]= p[1]
    elif(p.length == 4):
        p[0] = p[1] + p[2] + [p[3]] + p[4]
    return p[0]


def p_expression(p):
    '''
    expression : simple_expression
    | simple_expression EQ simple_expression”
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif (p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    return p[0]


def p_expression_list(p):
    '''
    expression_list : expression
    | expression_list COMA expression
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    return p[0]

def p_to_expression(p):
    '''
    to_expression : TO
    | DOWNTO
    '''
    p[0]=p[1]
    return p[0]

def p_for(p):
    '''
    for : FOR ID ASSIG expression to_expression expression
    '''
    p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6]
    return p[0]


def p_simple_expression(p):
    '''
    simple_expression : term
    | sign term
    | simple_expression PLUS term
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 2):
        p[0] = p[1] + p[2]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    return p[0]


def p_term(p):
    '''
    term : factor
    |term MULTIPLY factor
    '''
    if(p.length == 1):
        p[0] = p[1]
    elif(p.length == 3):
        p[0] = p[1] + p[2] + p[3]
    return p[0]


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
    return p[0]


def p_comp_operator(p):
    '''
    comp_operator : LT
    | GT
    | LOREQ
    | GOREQ
    | EQ
    '''
    p[0] = p[1]
    return p[0]

def p_comparison(p):
    '''
    comparison : num comp_operator num
    '''
    p[0] = p[1] + p[2] + p[3]
    return p[0]

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
    return p[0]

def p_logic_condition(p):
    '''
    logic_condition : NOT LP logic_statement statement_logic_operators logic_statement RP
    | LP logic_statement statement_logic_operators logic_statement RP
    '''
    if(p.length == 6):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5] + p[6]
    elif(p.length == 5):
        p[0] = p[1] + p[2] + p[3] + p[4] + p[5]
    return p[0]


parser = yacc.yacc()

