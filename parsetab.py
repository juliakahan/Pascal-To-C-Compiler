
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND ARRAY ASSIG BEGIN BINLSO BINRSO BOOLEAN CASE CHAR COL COMA CONST DECRBY DISPOSE DIV DIVBY DIVIDE DO DOLLAR DOT DOWNTO ELSE EMPTY END EOF EOLN EQ FALSE FILE FOR FUNCTION GET GOREQ GOTO GT HASH ID IF IN INCRBY INPUT INTEGER ISDIFF LABEL LBR LCURLBR LINEEND LOREQ LP LT MINUS MOD MULTBY MULTIPLY NIL NOT NUMBER OF OR OUTPUT PLUS PROCEDURE PROGRAM PUT RBR RCURLBR READ READLN REAL RECORD REF REPEAT RP SEMICOL SET SQRT SYMMDIFF TEXT THEN TO TRUE TYPE UNTIL VAR WHILE WITH WRITE WRITELN\n    pascal_program : program_id SEMICOL program_block DOT\n    \n    program_id : PROGRAM ID\n     \n    program_block : declared_type_block variable_block procedure_block operation_block\n    \n    declared_type_block : TYPE id_list\n    | empty\n    \n    variable_block : VAR var_decl var_decl_list\n    | empty\n    \n    var_decl : id_list COL type_denoter SEMICOL\n    \n    var_decl_list : var_decl_list var_decl\n    | empty\n    \n    id_list : ID\n    | id_list COMA ID\n    \n    procedure_block : procedure_block procedure\n    | empty\n    \n    procedure : procedure_header SEMICOL program_block SEMICOL\n    \n    procedure_header : PROCEDURE ID\n    | PROCEDURE ID LP parameters parameters_list RP\n    \n    parameters : id_list COL stand_type_procedure\n    \n    parameters_list : parameters_list SEMICOL parameters\n    | empty\n    \n    stand_type_procedure : CHAR\n    | INTEGER\n    | REAL\n    | BOOLEAN\n    \n    type_denoter : stand_type\n    | id1\n    \n        var_decl2 : ID\n        | id_list COMA ID\n        \n    sign : PLUS\n    | MINUS\n    \n    real_number : num\n    | num dot num\n    \n    num : NUMBER\n    \n    dot : DOT\n    \n    types : stand_type\n    | id1\n    \n    stand_type :  CHAR\n    | INTEGER\n    | REAL\n    | BOOLEAN\n    | TEXT\n    \n    end_sign : END\n   \n    statement_sequence : statement statement_list\n\n    \n\n    statement_list : statement_list SEMICOL statement\n    | empty\n    \n    variable : ID\n    | ID LBR expression RBR\n    \n    assign_statement : id2 ASSIG expression\n    \n    statement : simple_statement\n    | structured_statement\n    | for_statement\n    \n    simple_statement : assign_statement\n    | empty\n    \n    structured_statement : operation_sub_block\n    | conditional_statement\n    | loop_statement\n    \n    loop_statement : for_do\n    \n    for_statement : for_id ASSIG expression to_downto expression for_do statement\n    \n    comp_statement : BEGIN statement_sequence SEMICOL END\n    \n   operation_block : BEGIN statement_sequence END\n    \n    conditional_statement : if_part expression then_part statement else_part\n    | if_part expression then_part statement\n    \n    if_part : IF\n    \n    then_part : THEN\n    \n    bool_value : TRUE\n    | FALSE\n       \n    else_part : else statement\n    \n    else : ELSE\n    \n    id1 : ID\n    \n    id2 : ID\n    \n    id3 : ID\n    \n    expression : simple_expression comparison_operator simple_expression\n    | simple_expression\n    | bool_value\n    \n    operation_sub_block : BEGIN statement_sequence END\n    \n    for_id : FOR ID\n    \n    to_downto : TO\n    | DOWNTO\n    \n    for_do : DO\n    \n    comparison_operator : LT\n    | GT\n    | LOREQ\n    | GOREQ\n    | EQ\n    \n    simple_expression : term additional_oper_list\n    \n    additional_oper_list : additional_oper_list additional_oper term\n    | empty\n    \n    additional_oper : MINUS\n    | PLUS\n    | OR\n    \n    term : factor factor_list\n    \n    factor_list : factor_list and_div_oper factor\n    | empty\n    \n    and_div_oper : DIV\n    | AND\n    \n    factor : real_number\n    | ID\n    | LP expression RP\n    | NOT factor\n    \n    empty :\n    \n    comma : COMA\n    '
    
_lr_action_items = {'PROGRAM':([0,],[3,]),'$end':([1,10,],[0,-1,]),'SEMICOL':([2,5,21,23,24,30,32,33,34,35,36,37,38,39,40,42,48,50,52,53,54,55,56,57,58,59,60,62,63,64,68,69,70,71,72,73,74,75,78,79,81,84,85,87,88,89,96,97,98,99,101,105,107,111,112,120,121,122,123,126,127,128,129,130,131,133,134,135,136,137,138,139,140,141,],[4,-2,-3,-100,49,-100,-100,-49,-50,-51,-52,-53,-54,-55,-56,-57,-79,-16,83,-25,-26,-37,-38,-39,-40,-41,-69,-60,85,-45,-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,104,-75,-100,-48,-100,-64,-85,-87,-91,-93,-99,-100,-44,-62,-72,-98,-32,132,-20,-61,-100,-68,-86,-92,-17,-18,-21,-22,-23,-24,-100,-67,-19,-58,]),'ID':([3,8,12,18,20,23,25,26,27,28,30,44,45,47,48,51,65,66,76,77,82,83,85,88,89,90,91,92,93,94,95,108,109,110,113,114,115,116,117,118,119,127,128,132,138,],[5,15,15,-100,29,46,50,15,-10,60,46,75,80,-63,-79,-9,75,75,75,75,15,-8,46,46,-64,75,-80,-81,-82,-83,-84,75,-77,-78,75,-88,-89,-90,75,-94,-95,46,-68,15,46,]),'TYPE':([4,49,],[8,8,]),'VAR':([4,7,9,14,15,29,49,],[-100,12,-5,-4,-11,-12,-100,]),'BEGIN':([4,7,9,11,13,14,15,16,17,18,22,23,26,27,29,30,48,49,51,83,85,88,89,104,127,128,138,],[-100,-100,-5,-100,-7,-4,-11,23,-14,-100,-13,30,-6,-10,-12,30,-79,-100,-9,-8,30,30,-64,-15,30,-68,30,]),'PROCEDURE':([4,7,9,11,13,14,15,16,17,18,22,26,27,29,49,51,83,104,],[-100,-100,-5,-100,-7,-4,-11,25,-14,-100,-13,-6,-10,-12,-100,-9,-8,-15,]),'DOT':([6,21,62,78,79,],[10,-3,-60,103,-33,]),'COMA':([14,15,19,29,106,],[20,-11,20,-12,20,]),'COL':([15,19,29,106,],[-11,28,-12,124,]),'END':([23,30,31,32,33,34,35,36,37,38,39,40,42,48,61,63,64,68,69,70,71,72,73,74,75,78,79,84,85,87,88,89,96,97,98,99,101,107,111,112,120,121,126,127,128,129,130,138,139,141,],[-100,-100,62,-100,-49,-50,-51,-52,-53,-54,-55,-56,-57,-79,84,-43,-45,-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,-75,-100,-48,-100,-64,-85,-87,-91,-93,-99,-44,-62,-72,-98,-32,-61,-100,-68,-86,-92,-100,-67,-58,]),'FOR':([23,30,48,85,88,89,127,128,138,],[45,45,-79,45,45,-64,45,-68,45,]),'IF':([23,30,48,85,88,89,127,128,138,],[47,47,-79,47,47,-64,47,-68,47,]),'DO':([23,30,48,68,69,70,71,72,73,74,75,78,79,85,88,89,96,97,98,99,101,112,120,121,125,127,128,129,130,138,],[48,48,-79,-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,48,48,-64,-85,-87,-91,-93,-99,-72,-98,-32,48,48,-68,-86,-92,48,]),'CHAR':([28,124,],[55,134,]),'INTEGER':([28,124,],[56,135,]),'REAL':([28,124,],[57,136,]),'BOOLEAN':([28,124,],[58,137,]),'TEXT':([28,],[59,]),'ELSE':([33,34,35,36,37,38,39,40,42,48,68,69,70,71,72,73,74,75,78,79,84,87,88,89,96,97,98,99,101,111,112,120,121,126,127,128,129,130,138,139,141,],[-49,-50,-51,-52,-53,-54,-55,-56,-57,-79,-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,-75,-48,-100,-64,-85,-87,-91,-93,-99,128,-72,-98,-32,-61,-100,-68,-86,-92,-100,-67,-58,]),'ASSIG':([41,43,46,80,],[65,66,-70,-76,]),'TRUE':([44,47,65,66,76,108,109,110,],[71,-63,71,71,71,71,-77,-78,]),'FALSE':([44,47,65,66,76,108,109,110,],[72,-63,72,72,72,72,-77,-78,]),'LP':([44,47,50,65,66,76,77,90,91,92,93,94,95,108,109,110,113,114,115,116,117,118,119,],[76,-63,82,76,76,76,76,76,-80,-81,-82,-83,-84,76,-77,-78,76,-88,-89,-90,76,-94,-95,]),'NOT':([44,47,65,66,76,77,90,91,92,93,94,95,108,109,110,113,114,115,116,117,118,119,],[77,-63,77,77,77,77,77,-80,-81,-82,-83,-84,77,-77,-78,77,-88,-89,-90,77,-94,-95,]),'NUMBER':([44,47,65,66,76,77,90,91,92,93,94,95,102,103,108,109,110,113,114,115,116,117,118,119,],[79,-63,79,79,79,79,79,-80,-81,-82,-83,-84,79,-34,79,-77,-78,79,-88,-89,-90,79,-94,-95,]),'THEN':([67,68,69,70,71,72,73,74,75,78,79,96,97,98,99,101,112,120,121,129,130,],[89,-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,-85,-87,-91,-93,-99,-72,-98,-32,-86,-92,]),'TO':([68,69,70,71,72,73,74,75,78,79,86,96,97,98,99,101,112,120,121,129,130,],[-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,109,-85,-87,-91,-93,-99,-72,-98,-32,-86,-92,]),'DOWNTO':([68,69,70,71,72,73,74,75,78,79,86,96,97,98,99,101,112,120,121,129,130,],[-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,110,-85,-87,-91,-93,-99,-72,-98,-32,-86,-92,]),'RP':([68,69,70,71,72,73,74,75,78,79,96,97,98,99,100,101,105,112,120,121,122,123,129,130,133,134,135,136,137,140,],[-73,-74,-100,-65,-66,-100,-96,-97,-31,-33,-85,-87,-91,-93,120,-99,-100,-72,-98,-32,131,-20,-86,-92,-18,-21,-22,-23,-24,-19,]),'LT':([68,70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[91,-100,-100,-96,-97,-31,-33,-85,-87,-91,-93,-99,-98,-32,-86,-92,]),'GT':([68,70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[92,-100,-100,-96,-97,-31,-33,-85,-87,-91,-93,-99,-98,-32,-86,-92,]),'LOREQ':([68,70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[93,-100,-100,-96,-97,-31,-33,-85,-87,-91,-93,-99,-98,-32,-86,-92,]),'GOREQ':([68,70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[94,-100,-100,-96,-97,-31,-33,-85,-87,-91,-93,-99,-98,-32,-86,-92,]),'EQ':([68,70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[95,-100,-100,-96,-97,-31,-33,-85,-87,-91,-93,-99,-98,-32,-86,-92,]),'MINUS':([70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[-100,-100,-96,-97,-31,-33,114,-87,-91,-93,-99,-98,-32,-86,-92,]),'PLUS':([70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[-100,-100,-96,-97,-31,-33,115,-87,-91,-93,-99,-98,-32,-86,-92,]),'OR':([70,73,74,75,78,79,96,97,98,99,101,120,121,129,130,],[-100,-100,-96,-97,-31,-33,116,-87,-91,-93,-99,-98,-32,-86,-92,]),'DIV':([73,74,75,78,79,98,99,101,120,121,130,],[-100,-96,-97,-31,-33,118,-93,-99,-98,-32,-92,]),'AND':([73,74,75,78,79,98,99,101,120,121,130,],[-100,-96,-97,-31,-33,119,-93,-99,-98,-32,-92,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'pascal_program':([0,],[1,]),'program_id':([0,],[2,]),'program_block':([4,49,],[6,81,]),'declared_type_block':([4,49,],[7,7,]),'empty':([4,7,11,18,23,30,32,49,70,73,85,88,105,127,138,],[9,13,17,27,37,37,64,9,97,99,37,37,123,37,37,]),'variable_block':([7,],[11,]),'id_list':([8,12,26,82,132,],[14,19,19,106,106,]),'procedure_block':([11,],[16,]),'var_decl':([12,26,],[18,51,]),'operation_block':([16,],[21,]),'procedure':([16,],[22,]),'procedure_header':([16,],[24,]),'var_decl_list':([18,],[26,]),'statement_sequence':([23,30,],[31,61,]),'statement':([23,30,85,88,127,138,],[32,32,107,111,139,141,]),'simple_statement':([23,30,85,88,127,138,],[33,33,33,33,33,33,]),'structured_statement':([23,30,85,88,127,138,],[34,34,34,34,34,34,]),'for_statement':([23,30,85,88,127,138,],[35,35,35,35,35,35,]),'assign_statement':([23,30,85,88,127,138,],[36,36,36,36,36,36,]),'operation_sub_block':([23,30,85,88,127,138,],[38,38,38,38,38,38,]),'conditional_statement':([23,30,85,88,127,138,],[39,39,39,39,39,39,]),'loop_statement':([23,30,85,88,127,138,],[40,40,40,40,40,40,]),'for_id':([23,30,85,88,127,138,],[41,41,41,41,41,41,]),'for_do':([23,30,85,88,125,127,138,],[42,42,42,42,138,42,42,]),'id2':([23,30,85,88,127,138,],[43,43,43,43,43,43,]),'if_part':([23,30,85,88,127,138,],[44,44,44,44,44,44,]),'type_denoter':([28,],[52,]),'stand_type':([28,],[53,]),'id1':([28,],[54,]),'statement_list':([32,],[63,]),'expression':([44,65,66,76,108,],[67,86,87,100,125,]),'simple_expression':([44,65,66,76,90,108,],[68,68,68,68,112,68,]),'bool_value':([44,65,66,76,108,],[69,69,69,69,69,]),'term':([44,65,66,76,90,108,113,],[70,70,70,70,70,70,129,]),'factor':([44,65,66,76,77,90,108,113,117,],[73,73,73,73,101,73,73,73,130,]),'real_number':([44,65,66,76,77,90,108,113,117,],[74,74,74,74,74,74,74,74,74,]),'num':([44,65,66,76,77,90,102,108,113,117,],[78,78,78,78,78,78,121,78,78,78,]),'then_part':([67,],[88,]),'comparison_operator':([68,],[90,]),'additional_oper_list':([70,],[96,]),'factor_list':([73,],[98,]),'dot':([78,],[102,]),'parameters':([82,132,],[105,140,]),'to_downto':([86,],[108,]),'additional_oper':([96,],[113,]),'and_div_oper':([98,],[117,]),'parameters_list':([105,],[122,]),'else_part':([111,],[126,]),'else':([111,],[127,]),'stand_type_procedure':([124,],[133,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> pascal_program","S'",1,None,None,None),
  ('pascal_program -> program_id SEMICOL program_block DOT','pascal_program',4,'p_pascal_program','main.py',209),
  ('program_id -> PROGRAM ID','program_id',2,'p_program_id','main.py',217),
  ('program_block -> declared_type_block variable_block procedure_block operation_block','program_block',4,'p_program_block','main.py',227),
  ('declared_type_block -> TYPE id_list','declared_type_block',2,'p_declared_type_block','main.py',235),
  ('declared_type_block -> empty','declared_type_block',1,'p_declared_type_block','main.py',236),
  ('variable_block -> VAR var_decl var_decl_list','variable_block',3,'p_variable_block','main.py',243),
  ('variable_block -> empty','variable_block',1,'p_variable_block','main.py',244),
  ('var_decl -> id_list COL type_denoter SEMICOL','var_decl',4,'p_var_decl','main.py',251),
  ('var_decl_list -> var_decl_list var_decl','var_decl_list',2,'p_var_decl_list','main.py',272),
  ('var_decl_list -> empty','var_decl_list',1,'p_var_decl_list','main.py',273),
  ('id_list -> ID','id_list',1,'p_id_list','main.py',279),
  ('id_list -> id_list COMA ID','id_list',3,'p_id_list','main.py',280),
  ('procedure_block -> procedure_block procedure','procedure_block',2,'p_procedure_block','main.py',289),
  ('procedure_block -> empty','procedure_block',1,'p_procedure_block','main.py',290),
  ('procedure -> procedure_header SEMICOL program_block SEMICOL','procedure',4,'p_procedure','main.py',296),
  ('procedure_header -> PROCEDURE ID','procedure_header',2,'p_procedure_header','main.py',302),
  ('procedure_header -> PROCEDURE ID LP parameters parameters_list RP','procedure_header',6,'p_procedure_header','main.py',303),
  ('parameters -> id_list COL stand_type_procedure','parameters',3,'p_parameters','main.py',320),
  ('parameters_list -> parameters_list SEMICOL parameters','parameters_list',3,'p_parameters_list','main.py',335),
  ('parameters_list -> empty','parameters_list',1,'p_parameters_list','main.py',336),
  ('stand_type_procedure -> CHAR','stand_type_procedure',1,'p_stand_type_procedure','main.py',345),
  ('stand_type_procedure -> INTEGER','stand_type_procedure',1,'p_stand_type_procedure','main.py',346),
  ('stand_type_procedure -> REAL','stand_type_procedure',1,'p_stand_type_procedure','main.py',347),
  ('stand_type_procedure -> BOOLEAN','stand_type_procedure',1,'p_stand_type_procedure','main.py',348),
  ('type_denoter -> stand_type','type_denoter',1,'p_type_denoter','main.py',363),
  ('type_denoter -> id1','type_denoter',1,'p_type_denoter','main.py',364),
  ('var_decl2 -> ID','var_decl2',1,'p_var_decl2','main.py',371),
  ('var_decl2 -> id_list COMA ID','var_decl2',3,'p_var_decl2','main.py',372),
  ('sign -> PLUS','sign',1,'p_sign','main.py',404),
  ('sign -> MINUS','sign',1,'p_sign','main.py',405),
  ('real_number -> num','real_number',1,'p_real_number','main.py',413),
  ('real_number -> num dot num','real_number',3,'p_real_number','main.py',414),
  ('num -> NUMBER','num',1,'p_num','main.py',420),
  ('dot -> DOT','dot',1,'p_dot','main.py',429),
  ('types -> stand_type','types',1,'p_types','main.py',438),
  ('types -> id1','types',1,'p_types','main.py',439),
  ('stand_type -> CHAR','stand_type',1,'p_stand_type','main.py',447),
  ('stand_type -> INTEGER','stand_type',1,'p_stand_type','main.py',448),
  ('stand_type -> REAL','stand_type',1,'p_stand_type','main.py',449),
  ('stand_type -> BOOLEAN','stand_type',1,'p_stand_type','main.py',450),
  ('stand_type -> TEXT','stand_type',1,'p_stand_type','main.py',451),
  ('end_sign -> END','end_sign',1,'p_end_sign','main.py',475),
  ('statement_sequence -> statement statement_list','statement_sequence',2,'p_statement_sequence','main.py',517),
  ('statement_list -> statement_list SEMICOL statement','statement_list',3,'p_statement_list','main.py',526),
  ('statement_list -> empty','statement_list',1,'p_statement_list','main.py',527),
  ('variable -> ID','variable',1,'p_variable','main.py',538),
  ('variable -> ID LBR expression RBR','variable',4,'p_variable','main.py',539),
  ('assign_statement -> id2 ASSIG expression','assign_statement',3,'p_assign_statement','main.py',552),
  ('statement -> simple_statement','statement',1,'p_statement','main.py',560),
  ('statement -> structured_statement','statement',1,'p_statement','main.py',561),
  ('statement -> for_statement','statement',1,'p_statement','main.py',562),
  ('simple_statement -> assign_statement','simple_statement',1,'p_simple_statement','main.py',569),
  ('simple_statement -> empty','simple_statement',1,'p_simple_statement','main.py',570),
  ('structured_statement -> operation_sub_block','structured_statement',1,'p_structured_statement','main.py',577),
  ('structured_statement -> conditional_statement','structured_statement',1,'p_structured_statement','main.py',578),
  ('structured_statement -> loop_statement','structured_statement',1,'p_structured_statement','main.py',579),
  ('loop_statement -> for_do','loop_statement',1,'p_loop_statement','main.py',586),
  ('for_statement -> for_id ASSIG expression to_downto expression for_do statement','for_statement',7,'p_for_statement','main.py',593),
  ('comp_statement -> BEGIN statement_sequence SEMICOL END','comp_statement',4,'p_comp_statement','main.py',600),
  ('operation_block -> BEGIN statement_sequence END','operation_block',3,'p_operation_block','main.py',608),
  ('conditional_statement -> if_part expression then_part statement else_part','conditional_statement',5,'p_conditional_statement','main.py',625),
  ('conditional_statement -> if_part expression then_part statement','conditional_statement',4,'p_conditional_statement','main.py',626),
  ('if_part -> IF','if_part',1,'p_if_part','main.py',632),
  ('then_part -> THEN','then_part',1,'p_then_part','main.py',641),
  ('bool_value -> TRUE','bool_value',1,'p_bool_value','main.py',647),
  ('bool_value -> FALSE','bool_value',1,'p_bool_value','main.py',648),
  ('else_part -> else statement','else_part',2,'p_else_part','main.py',657),
  ('else -> ELSE','else',1,'p_else','main.py',664),
  ('id1 -> ID','id1',1,'p_id1','main.py',676),
  ('id2 -> ID','id2',1,'p_id2','main.py',684),
  ('id3 -> ID','id3',1,'p_id3','main.py',693),
  ('expression -> simple_expression comparison_operator simple_expression','expression',3,'p_expression','main.py',699),
  ('expression -> simple_expression','expression',1,'p_expression','main.py',700),
  ('expression -> bool_value','expression',1,'p_expression','main.py',701),
  ('operation_sub_block -> BEGIN statement_sequence END','operation_sub_block',3,'p_operation_sub_block','main.py',707),
  ('for_id -> FOR ID','for_id',2,'p_for_id','main.py',715),
  ('to_downto -> TO','to_downto',1,'p_to_downto','main.py',723),
  ('to_downto -> DOWNTO','to_downto',1,'p_to_downto','main.py',724),
  ('for_do -> DO','for_do',1,'p_for_do','main.py',737),
  ('comparison_operator -> LT','comparison_operator',1,'p_comparison_operator','main.py',748),
  ('comparison_operator -> GT','comparison_operator',1,'p_comparison_operator','main.py',749),
  ('comparison_operator -> LOREQ','comparison_operator',1,'p_comparison_operator','main.py',750),
  ('comparison_operator -> GOREQ','comparison_operator',1,'p_comparison_operator','main.py',751),
  ('comparison_operator -> EQ','comparison_operator',1,'p_comparison_operator','main.py',752),
  ('simple_expression -> term additional_oper_list','simple_expression',2,'p_simple_expression','main.py',768),
  ('additional_oper_list -> additional_oper_list additional_oper term','additional_oper_list',3,'p_additional_oper_list','main.py',775),
  ('additional_oper_list -> empty','additional_oper_list',1,'p_additional_oper_list','main.py',776),
  ('additional_oper -> MINUS','additional_oper',1,'p_additional_oper','main.py',783),
  ('additional_oper -> PLUS','additional_oper',1,'p_additional_oper','main.py',784),
  ('additional_oper -> OR','additional_oper',1,'p_additional_oper','main.py',785),
  ('term -> factor factor_list','term',2,'p_term','main.py',793),
  ('factor_list -> factor_list and_div_oper factor','factor_list',3,'p_factor_list','main.py',800),
  ('factor_list -> empty','factor_list',1,'p_factor_list','main.py',801),
  ('and_div_oper -> DIV','and_div_oper',1,'p_and_div_oper','main.py',808),
  ('and_div_oper -> AND','and_div_oper',1,'p_and_div_oper','main.py',809),
  ('factor -> real_number','factor',1,'p_factor','main.py',820),
  ('factor -> ID','factor',1,'p_factor','main.py',821),
  ('factor -> LP expression RP','factor',3,'p_factor','main.py',822),
  ('factor -> NOT factor','factor',2,'p_factor','main.py',823),
  ('empty -> <empty>','empty',0,'p_empty','main.py',835),
  ('comma -> COMA','comma',1,'p_comma','main.py',853),
]
