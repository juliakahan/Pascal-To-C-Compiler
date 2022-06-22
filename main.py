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
        'dispose': 'DISPOSE',
        'integer': 'INTEGER',
        'char': 'CHAR',
        'real': 'REAL',
        'boolean': 'BOOLEAN',

}
tokens = [
        'EMPTY',
        'ID',
        'PLUS',
        'MINUS',
        'MULTIPLY',
        'DIVIDE',
        'EQ',
        'LT',
        'GT',
        'DOT',
        'COMA',
        'REF',
        'DOLLAR',
        'HASH',
        'BINLSO',
        'BINRSO',
        'ISDIFF',
        'SYMMDIFF',
        'LOREQ',
        'GOREQ',
        'ASSIG',
        'INCRBY',
        'DECRBY',
        'MULTBY',
        'DIVBY',
        'LP',
        'RP',
        'LBR',
        'RBR',
        'SEMICOL',
        'COL',
        'LCURLBR',
        'RCURLBR',
        'TEXT',
        'NUMBER'

] + list(reserved.values())
#'LCOMM', 'RCOMM', 'LGROUP', 'RGROUP',
#, 'EXPON'
# Operators

t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_COMA = r','
t_DOT = r'\.'
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


t_INTEGER = r'^[1-9][0-9]*|0$'
t_CHAR= r'(L)?\'([^\\\n]|(\\.))*?\''
#t_REAL = r'((\+|-)?([0-9]+)(\.[0-9]+)?)|((\+|-)?\.?[0-9]+)'
# t_BOOLEAN = r'true|false'
t_EMPTY = r'""'


def t_NUMBER(t):
    r'''[0-9][0-9]*'''
    t.value = int(t.value)
    return t

def t_BOOLEAN(t):
    r''' true | false '''
    t.value = bool(t.value)


def t_newline(t):
    r'''\n+'''
    t.lexer.lineno += len(t.value)

def t_ID(t):
    r'''[a-zA-Z][a-zA-Z0-9]*'''
    t.type = reserved.get(t.value, 'ID')
    return t

def t_TEXT(t):
    r'''\'[^\'\n]*\''''
    return t

t_ignore = '  \t'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

#======================================================================================================================================================

with open('test3_correct_syntax') as f:
    lines = f.readlines()
content = "".join(lines)

lexer.input(content)
for token in lexer:
    print("line %d: %s(%s)" %(token.lineno, token.type, token.value))


C_code = ""
ID_list = []
declar = []
subprogram_decl = []
comp_stats = []
list_statement = []
statements_list = []
id_decl = {}
type_decl = {}
procedure_type = []
temp_ID_list = []
function_name=""



def p_pascal_program(p):
    '''
    pascal_program : program_id SEMICOL program_block DOT
    '''
    print("program")
    pass


def p_program_id(p):
    '''
    program_id : PROGRAM ID
     '''
    global output
    output += "#include <stdio.h>\n"
    output += "int main() {\n"
    print("programID")


def p_program_block(p):
    '''
    program_block : declared_type_block variable_block procedure_block operation_block
    '''
    print("block")
    pass


def p_declared_type_block(p):
    '''
    declared_type_block : TYPE id_list
    | empty
    '''
    print("type_block")
    pass

def p_variable_block(p):
    '''
    variable_block : VAR var_decl var_decl_list
    | empty
    '''
    print("var_block")
    pass

def p_var_decl(p):
    '''
    var_decl : id_list COL type_denoter SEMICOL
    '''
    global output
    for i in ID_list:
        if i != ID_list[-1]:
            if varType == "char*":
                output += "char "+ i + "[100], "
            else:
                output += i + ", "
        else:
            if varType == "char*":
                output += "char "+ i + "[100];\n"
            else:
                output += i + ";\n"
        id_decl[str(i)] = varType

    ID_list.clear()
    print("id_decl")

def p_var_decl_list(p):
    '''
    var_decl_list : var_decl_list var_decl
    | empty
    '''
    pass

def p_id_list(p):
    '''
    id_list : ID
    | id_list COMA ID
    '''
    if p[1] is not None:
        ID_list.append(p[1])
    else:
        ID_list.append(p[3])

def p_procedure_block(p):
    '''
    procedure_block : procedure_block procedure
    | empty
    '''
    pass

def p_procedure(p):
    '''
    procedure : procedure_header SEMICOL program_block SEMICOL
    '''


def p_procedure_header(p):
    '''
    procedure_header : PROCEDURE ID
    | PROCEDURE ID LP parameters parameters_list RP
    '''
    global output
    output += "void " + p[2] + "("
    for i in temp_ID_list:
        if i != temp_ID_list[-1]:
            output += i + ", "
        else:
            output += i
    output += ")" + "\n"
    temp_ID_list.clear()
    procedure_type.clear()
    ID_list.clear()


def p_parameters(p):
    '''
    parameters : id_list COL stand_type_procedure
    '''
    id_pom = []
    for i in ID_list:
        st = procedure_type[-1] + " " + i
        id_pom.append(st)
    ID_list.clear()
    for i in id_pom:
        ID_list.append(i)
    procedure_type.clear()
    print("param")


def p_parameters_list(p):
    '''
    parameters_list : parameters_list SEMICOL parameters
    | empty
    '''
    for i in ID_list:
        temp_ID_list.append(i)
    ID_list.clear()


def p_stand_type_procedure(p):
    '''
    stand_type_procedure : CHAR
    | INTEGER
    | REAL
    | BOOLEAN
    '''
    if p[1] == 'char':
        procedure_type.append("char")
    if p[1] == 'integer':
        procedure_type.append("int")
    elif p[1] == 'real':
        procedure_type.append("double")
    elif p[1] == 'boolean':
        procedure_type.append("bool")
    print("stand_type")


def p_type_denoter(p):
    '''
    type_denoter : stand_type
    | id1
    '''
    pass


def p_var_decl2(p):
    '''
        var_decl2 : ID
        | id_list COMA ID
        '''
    global output
    if len(p) == 3:
        for id in [p[1]]:
            ID_list.append(id)
            if [p[len(p) - 1]] == id:
                output += id
            else:
                output += id + ","
    elif len(p) == 1:
        ID_list.append(p[1])
        output += p[1]
    print("programVarDecl2")









#==================================================some helpful grammar definitions============================================






def p_sign(p):
    '''
    sign : PLUS
    | MINUS
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + str(p[1])


def p_real_number(p):
    '''
    real_number : num
    | num dot num
    '''


def p_num(p):
    '''
    num : NUMBER
    '''

    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + str(p[1])


def p_dot(p):
    '''
    dot : DOT
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + str(p[1])



def p_types(p):
    '''
    types : stand_type
    | id1
    '''
    pass



def p_stand_type(p):
    '''
    stand_type :  CHAR
    | INTEGER
    | REAL
    | BOOLEAN
    | TEXT
    '''
    global output, varType
    if p[1] == 'integer':
        output += "int "
        varType = "int"
    elif p[1] == 'real':
        output += "double "
        varType = "double"
    elif p[1] == 'char':
        output += "char "
        varType = "char"
    elif p[1] == 'boolean':
        output += "bool "
        varType = "bool"
    elif p[1] == 'string':
        output += "char "
        varType = "char*"
    print("var_type")



def p_end_sign(p):
    '''
    end_sign : END
   '''
    global output
    output += "return 0; \n"
    output += "}"
    print("end")









# def p_declarations(p):
#     '''
#     declarations : VAR ID_list COL type SEMICOL
#     | opt_declarations
#     '''
#     global output
# #
#     if(len(p) == 5):
#         output += ID_list[0].type
#         for id in ID_list:
#             if ID_list[len(ID_list) - 1] == id:
#                 output += id + ";\n"
#             else:
#                 output += id + ","
#     print("DECLARATION")
#     pass
#
# def p_type(p):
#     '''
#     type : standard_type
#     '''
#     p[0] = p[1]
#     print("type")
#
#
def p_statement_sequence(p):
    '''
    statement_sequence : statement statement_list

    '''
    pass
    print("sequence")

def p_statement_list(p):
    '''

    statement_list : statement_list SEMICOL statement
    | empty
    '''
    # if p[1] is not None:
    #     ID_list.append(p[1])
    #
    # else:
    #     ID_list.append(p[3])


def p_variable(p):
    '''
    variable : ID
    | ID LBR expression RBR
    '''
    print("variable")
    global output
    if len(p) == 1:
        output += p[1]
    elif len(p) == 4:
        output += p[1] + " (" + p[3] + ") "



def p_assign_statement(p):
    '''
    assign_statement : id2 ASSIG expression
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + ";"
    print("assign statement")

def p_statement(p):
    '''
    statement : simple_statement
    | structured_statement
    | for_statement
    '''
    pass
    print("statement")

def p_simple_statement(p):
    '''
    simple_statement : assign_statement
    | empty
    '''
    print("simple statement")

#| procedure_statement
def p_structured_statement(p):
    '''
    structured_statement : operation_sub_block
    | conditional_statement
    | loop_statement
    '''
    pass


def p_loop_statement(p):
    '''
    loop_statement : for_do
    '''



def p_for_statement(p):
    '''
    for_statement : for_id ASSIG expression to_downto expression for_do statement
    '''
    pass


def p_comp_statement(p):
    '''
    comp_statement : BEGIN statement_sequence SEMICOL END
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1]  #+ "\n\t}"
    print("comp")

def p_operation_block(p):
    '''
   operation_block : BEGIN statement_sequence END
    '''
    global output, function_name

   # output += "{\n"
    for st in statements_list:
        output +=  st + "\n"
    if function_name != "":
        output += "return "+function_name+";\n"
        function_name=""
    output += "return 0; \n"
    output += "}\n"
    statements_list.clear()
    print("op_block")

def p_conditional_statement(p):
    '''
    conditional_statement : if_part expression then_part statement else_part
    | if_part expression then_part statement
    '''
    pass

def p_if_part(p):
    '''
    if_part : IF
    '''
    if len(statements_list) > 0 and len(statements_list[-1]) > 6 and statements_list[-1][-4:] == 'else':
        statements_list[-1]  =  str(statements_list[-1]).replace("else", "else if(")
    else:
        statements_list.append("if (")

def p_then_part(p):
    '''
    then_part : THEN
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + ")"
def p_bool_value(p):
    '''
    bool_value : TRUE
    | FALSE
       '''
    if p[1] == 'true':
        output += 'True'
    else:
        output += 'False'

def p_else_part(p):
    '''
    else_part : else statement
    '''
    pass


def p_else(p):
    '''
    else : ELSE
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + "\n\telse"






def p_id1(p):
    '''
    id1 : ID
    '''
    global output
    output += p[1] + " "


def p_id2(p):
    '''
    id2 : ID
    '''
    if len(statements_list) > 0 and statements_list[-1][-1] == "\t":
        statements_list[-1] = statements_list[-1] + str(p[1])+" = "
    else:
        statements_list.append(str(p[1]) + " = ")

def p_id3(p):
    '''
    id3 : ID
    '''
    statements_list.append(str(p[1]) + "(")

def p_expression(p):
    '''
    expression : simple_expression comparison_operator simple_expression
    | simple_expression
    | bool_value
    '''
    pass

def p_operation_sub_block(p):
    '''
    operation_sub_block : BEGIN statement_sequence END
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + "\n\t}"
    print("subblock")

def p_for_id(p):
    '''
    for_id : FOR ID
    '''
    global  id_in_loop
    statements_list.append("for("+str(p[2])+"=")
    id_in_loop = str(p[2])

def p_to_downto(p):
    '''
    to_downto : TO
    | DOWNTO
    '''
    global id_in_loop
    if len(statements_list) > 0:
        if p[1]=="to":
            statements_list[-1] = statements_list[-1] +";"+id_in_loop+"<="
            id_in_loop = id_in_loop+"+"
        else:
            statements_list[-1] = statements_list[-1] + ";" + id_in_loop + ">="
            id_in_loop = id_in_loop + "-"

def p_for_do(p):
    '''
    for_do : DO
    '''
    if len(statements_list) > 0:
        if id_in_loop[-1] == "+":
            statements_list[-1] = statements_list[-1] +";++"+id_in_loop[:-1]+"){\n\t"
        else:
            statements_list[-1] = statements_list[-1] + ";--" + id_in_loop[:-1]+"){\n\t"


def p_comparison_operator(p):
    '''
    comparison_operator : LT
    | GT
    | LOREQ
    | GOREQ
    | EQ
    '''

    global output
    output += p[1]
    if p[1] is not None and len(statements_list) > 0:
        if p[1] == "=":
            statements_list[-1] = statements_list[-1] + "=="
        elif p[1] == "<>":
            statements_list[-1] = statements_list[-1] + "!="
        else:
            statements_list[-1] = statements_list[-1] + str(p[1])


def p_simple_expression(p):
    '''
    simple_expression : term additional_oper_list
    '''
    pass


def p_additional_oper_list(p):
    '''
    additional_oper_list : additional_oper_list additional_oper term
    | empty
    '''
    pass


def p_additional_oper(p):
    '''
    additional_oper : MINUS
    | PLUS
    | OR
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1] + str(p[1])


def p_term(p):
    '''
    term : factor factor_list
    '''
    pass


def p_factor_list(p):
    '''
    factor_list : factor_list and_div_oper factor
    | empty
    '''
    pass


def p_and_div_oper(p):
    '''
    and_div_oper : DIV
    | AND
    '''
    if len(statements_list) > 0:
        if p[1] == 'div':
            statements_list[-1] = statements_list[-1] + "/"
        elif p[1] == 'and':
            statements_list[-1] = statements_list[-1] + "&&"


def p_factor(p):
    '''
    factor : real_number
    | ID
    | LP expression RP
    | NOT factor
    '''
    if p[1] is not None and p[1] != "(" and p[1] != "not":
        if len(statements_list) > 0:
            statements_list[-1] = statements_list[-1] + str(p[1])
    print("factor")




def p_empty(p):
    '''
    empty :
    '''
    print("empty")
    pass




# def p_error(p):
#     raise Exception("Syntax error at '{}' at line: {}.\n".format(p.value, p.lexer.lineno))

def p_error(p):
    print("Syntax error at '%s'\n" % p.value)
    global wasError
    wasError = True

def p_comma(p):
    '''
    comma : COMA
    '''
    if len(statements_list) > 0:
        statements_list[-1] = statements_list[-1]+str(p[1])






#==============================================================ADD YOUR EXAMPLE==============================================================
global output
output = ""
with open('myPascalCode') as f:  #introduce name of your file
    lines = f.readlines()
code = "".join(lines)
parser = yacc.yacc()
parser.parse(code)
with open('conversionResult', 'w') as file:
    file.write(output)





