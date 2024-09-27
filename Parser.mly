%{
(*
 Alan Compiler
 Copyright (C) 2024  Andreas Stamos, Harris Platanos
 
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


        open Ast
%}

%token T_eof
%token<int> T_const
%token<string> T_id
%token<char> T_char
%token<string> T_string
%token T_byte
%token T_else
%token T_false
%token T_if
%token T_int
%token T_proc
%token T_reference
%token T_return
%token T_while
%token T_true
%token T_assign
%token T_lparen
%token T_rparen
%token T_lbrack
%token T_rbrack
%token T_lcbrack
%token T_rcbrack
%token T_comma
%token T_colon
%token T_semicol
%token T_or
%token T_and
%token T_eq
%token T_neq
%token T_geq
%token T_leq
%token T_less
%token T_gr
%token T_plus
%token T_minus
%token T_times
%token T_div
%token T_mod
%token T_pos
%token T_neg
%token T_not

%token T_then

%left T_or
%left T_and
%nonassoc T_eq T_neq T_less T_gr T_leq T_geq
%left T_plus T_minus
%left T_times T_div T_mod
%nonassoc T_pos T_neg T_not

/* this is for the dangling else */
%nonassoc T_then
%nonassoc T_else


%start program
%start declarations

%type <Ast.funcDecl list> declarations
%type <funcDecl>        func_decl
%type <ttype list>      fdecl_type_list
%type <ttype>           fdecl_type

%type <Ast.funcDef>     program
%type <varDef list>     fpar_list
%type <varDef>          fpar_def
%type <ttype>           data_type
%type <ttype>           ttype
%type <ttype>           r_type
%type <localDef list>   local_defs
%type <localDef>        local_def
%type <varDef>          var_def
%type <stmt list>       stmt_list
%type <stmt>            stmt
%type <stmt list>       compound_stmt
%type <func_call>       func_call
%type <expr list>       expr_list
%type <expr>            expr
%type <lvalue>          l_value
%type <cond>            cond



%%

declarations  : func_decl T_semicol declarations { $1 :: $3 }
              | func_decl T_semicol T_eof { [$1] }     

func_decl     : T_id T_lparen fdecl_type_list T_rparen T_colon r_type { { id = $1; params = $3; rtype = $6 } }
              | T_id T_lparen T_rparen T_colon r_type { { id = $1; params = []; rtype = $5 } }

fdecl_type_list : fdecl_type T_comma fdecl_type_list { $1::$3 }
                | fdecl_type { [$1] }

fdecl_type    : ttype { $1 }
              | T_reference ttype { Ref $2 }

/* here starts the Alan grammar */

program       : func_def T_eof { $1 }
              

func_def      : T_id T_lparen fpar_list T_rparen T_colon r_type local_defs compound_stmt { { id = $1; params = $3; rtype = $6; localdef = $7; compound_stmt = $8 } }
              | T_id T_lparen T_rparen T_colon r_type local_defs compound_stmt { { id = $1; params = []; rtype = $5; localdef = $6; compound_stmt = $7 } }


fpar_list     : fpar_def { [$1] }
              | fpar_def T_comma fpar_list { $1::$3 }

fpar_def      : T_id T_colon ttype { { id = $1; ttype = $3 } }
              | T_id T_colon T_reference ttype { { id = $1; ttype = Ref $4 } } 

data_type     : T_int { Int_t }
              | T_byte { Byte_t }

ttype         : data_type { $1 }
              | data_type T_lbrack T_rbrack { Array { ttype = $1; length = None } }

r_type        : data_type { $1 }
              | T_proc { Proc_t }

local_defs    : /* nothing */ { [] }
              | local_def local_defs { $1::$2 }

local_def     : func_def { LocalFun $1 }
              | var_def { LocalVar $1 }

var_def       : T_id T_colon data_type T_semicol { { id = $1; ttype = $3 } }
              | T_id T_colon data_type T_lbrack T_const T_rbrack T_semicol { { id = $1; ttype = Array { ttype = $3; length = Some $5 } } }

stmt_list     : /* nothing */ { [] }
              | stmt stmt_list { $1::$2 }
          
stmt          : T_semicol { Null }
              | l_value T_assign expr T_semicol { Assign { lvalue = $1; expr = $3 } }
              | compound_stmt { CompoundStmt $1 }
              | func_call T_semicol { FuncCall_s $1 }
              | T_if T_lparen cond T_rparen stmt %prec T_then { IfThenElse { cond = $3; tthen = $5; eelse = None } }   
              | T_if T_lparen cond T_rparen stmt T_else stmt { IfThenElse { cond = $3; tthen = $5; eelse = Some $7 } }
              | T_while T_lparen cond T_rparen stmt { While { cond = $3; stmt = $5 } }
              | T_return expr T_semicol { Return (Some $2) }
              | T_return T_semicol { Return None }

compound_stmt : T_lcbrack stmt_list T_rcbrack { $2 }

func_call     : T_id T_lparen T_rparen { { id = $1; params = [] } }
              | T_id T_lparen expr_list T_rparen { { id = $1; params = $3 } }

expr_list     : expr { [$1] }
              | expr T_comma expr_list { $1::$3 }

expr          : T_const { ConstInt $1 }
              | T_char { ConstChar $1 }
              | l_value { LValue_e $1 }
              | T_lparen expr T_rparen { $2 }
              | func_call { FuncCall_e $1 }
              | T_plus expr %prec T_pos { $2 } 
              | T_minus expr %prec T_neg { UnMinus $2 }
              | expr T_plus expr { Plus ($1,$3) }
              | expr T_minus expr { Minus ($1,$3) }
              | expr T_times expr { Mul ($1,$3) }
              | expr T_div expr { Div ($1,$3) }
              | expr T_mod expr { Mod ($1,$3) }

l_value       : T_id { Var $1 }
              | T_id T_lbrack expr T_rbrack { VarIdx ($1,$3) }
              | T_string { StrLiteral $1 } 

               
cond          : T_true { True_c }
              | T_false { False_c }
              | T_lparen cond T_rparen { $2 }
              | T_not cond { Not $2 }
              | expr T_eq expr { Eq ($1,$3) }
              | expr T_neq expr { Neq ($1,$3) }
              | expr T_geq expr { Geq ($1,$3) }
              | expr T_leq expr { Leq ($1,$3) }
              | expr T_less expr { Lt ($1,$3) }
              | expr T_gr expr { Gt ($1,$3) }
              | cond T_and cond { And ($1,$3) }
              | cond T_or cond { Or ($1,$3) }

