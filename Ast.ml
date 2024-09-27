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


type funcDecl = { id: string;
                 params: ttype list;
                 rtype: ttype;
                }

and funcDef = { id: string;
                 params: varDef list;
                 rtype: ttype;
                 localdef: localDef list;
                 compound_stmt: stmt list;
               }

and localDef = | LocalVar of varDef
               | LocalFun of funcDef

and varDef = { id: string; ttype: ttype }

and ttype = | Proc_t | Int_t | Byte_t 
            | Array of { ttype: ttype; length: int option }
            | Func of (ttype list) * ttype
            | Func2 of { param_types: ttype list; rtype: ttype; captured: string list }
            | Ref of ttype

and stmt = | CompoundStmt of stmt list
           | Assign of { lvalue: lvalue; expr: expr; }
           | FuncCall_s of func_call
           | IfThenElse of { cond: cond; tthen: stmt; eelse: stmt option }
           | While of { cond: cond; stmt: stmt }
           | Return of expr option
           | Null

and lvalue = | Var of string
             | VarIdx of string * expr
             | StrLiteral of string


and func_call = { id: string; params: expr list }

and expr = | ConstInt of int
           | ConstChar of char
           | LValue_e of lvalue
           | FuncCall_e of func_call
           | UnMinus of expr
           | Plus of expr * expr
           | Minus of expr * expr
           | Mul of expr * expr
           | Div of expr * expr
           | Mod of expr * expr

and cond = | True_c | False_c
           | Not of cond
           | Eq of expr * expr
           | Neq of expr * expr
           | Lt of expr * expr
           | Gt of expr * expr
           | Leq of expr * expr
           | Geq of expr * expr
           | And of cond * cond
           | Or of cond * cond

