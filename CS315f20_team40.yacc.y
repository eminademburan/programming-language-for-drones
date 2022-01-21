%{
  #include <stdio.h>
  int yylex();
  int yyerror();
%}
%token LP RP  BEGINE END IF ELSE WHILE FOR VOID FUNCTION DOES RETURN VAR CONST COMMENT INPUT OUTPUT PLUS MINUS MULT DIV SC ASSIGN_OP AND OR READINC READALT READTEMP READACC READTIME TOGGLECAM TAKEPIC CONNECTWIFI TAKEOF LAND CHANGEALT ROTATE GOFORWARD GOBACKWARD TRUE FALSE COMMA EQ GT GTE LT LTE NE IDENTIFIER STRING CHAR INT DOUBLE NOT
%%
s: BEGINE stmts END

stmts: stmt |stmt stmts

stmt: if_stmt | loop_stmt | funcall_stmt | assign_stmt | decl_stmt | input_stmt | output_stmt | comment_stmt

if_stmt: IF LP logical_exprs RP BEGINE stmts END | IF LP logical_exprs RP BEGINE stmts END ELSE BEGINE stmts END

comment_stmt: COMMENT

input_stmt: INPUT LP IDENTIFIER RP | INPUT LP IDENTIFIER COMMA print_expr RP

output_stmt: OUTPUT LP print_expr RP | OUTPUT LP print_expr COMMA print_expr RP 

print_expr: IDENTIFIER | primitive_value | IDENTIFIER PLUS print_expr | primitive_value PLUS print_expr | primretfun_call PLUS print_expr | returnfunc_call PLUS print_expr | primretfun_call | returnfunc_call

loop_stmt: while_loop | for_loop

while_loop: WHILE LP logical_exprs RP BEGINE stmts END
  
for_loop: FOR LP decl_stmt SC logical_exprs SC assign_stmt RP BEGINE stmts END

funcall_stmt: returnfunc_call | voidfunc_call | primretfun_call | primvoidfun_call 

returnfunc_call: IDENTIFIER LP call_param_list RP
voidfunc_call: VOID IDENTIFIER LP call_param_list RP

primretfun_call: readincl_call | readalt_call | readtemp_call | readacc_call | readtime_call

primvoidfun_call: togglecam_call | takepic_call | connectwifi_call | takeof_call | land_call | changealt_call | rotate_call | goforward_call | gobackward_call

readincl_call: READINC LP RP
readalt_call: READALT LP RP
readtemp_call: READTEMP LP RP
readacc_call: READACC LP RP
readtime_call: READTIME LP RP
togglecam_call: TOGGLECAM LP RP
takepic_call: TAKEPIC LP RP
connectwifi_call: CONNECTWIFI LP RP
takeof_call: TAKEOF LP RP
land_call: LAND LP RP
changealt_call: CHANGEALT LP primitive_value RP | CHANGEALT LP IDENTIFIER RP
rotate_call: ROTATE LP primitive_value RP | ROTATE LP IDENTIFIER RP
goforward_call: GOFORWARD LP primitive_value RP | GOFORWARD LP IDENTIFIER RP
gobackward_call: GOBACKWARD LP primitive_value RP | GOBACKWARD  LP IDENTIFIER RP

assign_stmt: IDENTIFIER ASSIGN_OP primretfun_call | IDENTIFIER ASSIGN_OP returnfunc_call | IDENTIFIER ASSIGN_OP expression | IDENTIFIER ASSIGN_OP STRING | IDENTIFIER ASSIGN_OP CHAR

decl_stmt: func_decl | var_decl | const_decl

func_decl: returnfunc_decl | voidfunc_decl

returnfunc_decl: FUNCTION IDENTIFIER LP dec_param_list RP DOES func_stmts END

voidfunc_decl: FUNCTION VOID IDENTIFIER LP dec_param_list RP DOES stmts END

func_stmts: func_stmt | func_stmt func_stmts

func_stmt: func_if_stmt | func_loop_stmt | funcall_stmt | assign_stmt | decl_stmt | return_stmt | output_stmt | input_stmt

func_if_stmt: IF LP logical_exprs RP BEGINE func_stmts END | IF LP logical_exprs RP BEGINE func_stmts END ELSE BEGINE func_stmts END

func_loop_stmt: func_while_loop | func_for_loop
func_while_loop: WHILE LP logical_exprs RP BEGINE func_stmts END
func_for_loop: FOR LP decl_stmt SC logical_exprs SC assign_stmt RP BEGINE func_stmts END

return_stmt: RETURN STRING | RETURN CHAR | RETURN expression | RETURN returnfunc_call | RETURN primretfun_call

dec_param_list: | IDENTIFIER | IDENTIFIER COMMA dec_param_list

call_param_list: | primitive_value | IDENTIFIER | returnfunc_call | primretfun_call | IDENTIFIER COMMA call_param_list | returnfunc_call COMMA call_param_list | primretfun_call COMMA call_param_list | primitive_value COMMA call_param_list

expression: logical_exprs | arith_expr

logical_exprs: logical_exprs OR logical_term | logical_term

logical_term: logical_term AND logical_expr | logical_expr

logical_expr: LP logical_exprs RP | comparison | TRUE | FALSE | NOT LP logical_exprs RP 

comparison: IDENTIFIER logic_op IDENTIFIER | primitive_value logic_op IDENTIFIER | IDENTIFIER logic_op primitive_value | returnfunc_call logic_op IDENTIFIER | IDENTIFIER logic_op returnfunc_call | returnfunc_call logic_op returnfunc_call | primitive_value logic_op returnfunc_call | returnfunc_call logic_op primitive_value | primretfun_call logic_op returnfunc_call | returnfunc_call logic_op primretfun_call | primretfun_call logic_op IDENTIFIER | IDENTIFIER logic_op primretfun_call | primretfun_call logic_op primitive_value | primitive_value logic_op primretfun_call | primretfun_call logic_op primretfun_call

logic_op: EQ | GT | GTE | LT | LTE | NE

arith_expr: arith_expr PLUS term | arith_expr MINUS term | term

term: term MULT base_term | term DIV base_term | base_term

base_term: LP arith_expr RP | INT | DOUBLE | IDENTIFIER

var_decl: VAR IDENTIFIER | VAR IDENTIFIER ASSIGN_OP returnfunc_call | VAR IDENTIFIER ASSIGN_OP primretfun_call | VAR IDENTIFIER ASSIGN_OP STRING | VAR IDENTIFIER ASSIGN_OP arith_expr | VAR IDENTIFIER ASSIGN_OP CHAR | VAR IDENTIFIER ASSIGN_OP TRUE | VAR IDENTIFIER ASSIGN_OP FALSE

const_decl: CONST IDENTIFIER ASSIGN_OP primitive_value

primitive_value: INT | DOUBLE | STRING | TRUE | FALSE | CHAR

%%
#include "lex.yy.c"
int lineno=1;
int yyerror(char *s) { printf("Syntax error on line %d !", lineno); 
                     }
int main() {
  if(yyparse()==0)
    printf("Input program is valid.\n");
  return 0;
}
