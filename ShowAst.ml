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

let rec show_expr = function
  | ConstInt i -> "ConstInt " ^ string_of_int i
  | ConstChar c -> "ConstChar '" ^ Char.escaped c ^ "'"
  | LValue_e (Var v) -> "LValue_e (Var \"" ^ v ^ "\")"
  | LValue_e (VarIdx (v, e)) -> "LValue_e (VarIdx (\"" ^ v ^ "\", " ^ show_expr e ^ "))"
  | LValue_e (StrLiteral v) -> "LValue_e (StrLiteral \"" ^ v ^ "\")"
  | FuncCall_e {id; params} -> 
      "FuncCall_e { id = \"" ^ id ^ "\"; params = [" ^ String.concat "; " (List.map show_expr params) ^ "] }"
  | UnMinus e -> "UnMinus (" ^ show_expr e ^ ")"
  | Plus (e1, e2) -> "Plus (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Minus (e1, e2) -> "Minus (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Mul (e1, e2) -> "Mul (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Div (e1, e2) -> "Div (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Mod (e1, e2) -> "Mod (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"

let rec show_cond = function
  | True_c -> "True_c"
  | False_c -> "False_c"
  | Not c -> "Not (" ^ show_cond c ^ ")"
  | Eq (e1, e2) -> "Eq (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Neq (e1, e2) -> "Neq (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Lt (e1, e2) -> "Lt (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Gt (e1, e2) -> "Gt (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Leq (e1, e2) -> "Leq (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | Geq (e1, e2) -> "Geq (" ^ show_expr e1 ^ ", " ^ show_expr e2 ^ ")"
  | And (c1, c2) -> "And (" ^ show_cond c1 ^ ", " ^ show_cond c2 ^ ")"
  | Or (c1, c2) -> "Or (" ^ show_cond c1 ^ ", " ^ show_cond c2 ^ ")"

let rec show_stmt = function
  | CompoundStmt stmts -> "CompoundStmt [" ^ String.concat "; " (List.map show_stmt stmts) ^ "]"
  | Assign {lvalue = Var v; expr} -> 
      "Assign { lvalue = Var \"" ^ v ^ "\"; expr = " ^ show_expr expr ^ " }"
  | Assign {lvalue = VarIdx (v, e1); expr = e2} -> 
      "Assign { lvalue = VarIdx (\"" ^ v ^ "\", " ^ show_expr e1 ^ "); expr = " ^ show_expr e2 ^ " }"
  | Assign {lvalue = StrLiteral v; expr = e2} -> 
      "Assign { lvalue = StrLiteral (\"" ^ v ^ "); expr = " ^ show_expr e2 ^ " }"
  | FuncCall_s {id; params} -> 
      "FuncCall_s { id = \"" ^ id ^ "\"; params = [" ^ String.concat "; " (List.map show_expr params) ^ "] }"
  | IfThenElse {cond; tthen; eelse} ->
      "IfThenElse { cond = " ^ show_cond cond ^ "; tthen = " ^ show_stmt tthen ^
      "; eelse = " ^ (match eelse with Some s -> "Some (" ^ show_stmt s ^ ")" | None -> "None") ^ " }"
  | While {cond; stmt} -> "While { cond = " ^ show_cond cond ^ "; stmt = " ^ show_stmt stmt ^ " }"
  | Return eopt -> "Return " ^ (match eopt with Some e -> "Some (" ^ show_expr e ^ ")" | None -> "None")
  | Null -> "Null"

let rec show_ttype = function
  | Int_t -> "Int_t"
  | Byte_t -> "Byte_t"
  | Proc_t -> "Proc_t"
  | Array {ttype; length} -> 
      "Array { ttype = " ^ show_ttype ttype ^ "; length = " ^ (match length with Some l -> string_of_int l | None -> "None") ^ " }"
  | Func (t1, t2) -> "Func (" ^ String.concat ", " (List.map show_ttype t1) ^ ", " ^ show_ttype t2 ^ ")"
  | Ref t -> "Ref (" ^ show_ttype t ^ ")"

let show_decl {id; ttype} = "{ id = \"" ^ id ^ "\"; ttype = " ^ show_ttype ttype ^ " }"

let rec show_localDef = function
  | LocalVar v -> "LocalVar (" ^ show_decl v ^ ")"
  | LocalFun f -> "LocalFun (" ^ show_funcDef f ^ ")"

and show_funcDef {id; params; rtype; localdef; compound_stmt} =
  "{ id = \"" ^ id ^ "\"; params = [" ^ String.concat "; " (List.map show_decl params) ^ 
  "]; rtype = " ^ show_ttype rtype ^ "; localdef = [" ^ String.concat "; " (List.map show_localDef localdef) ^ 
  "]; compound_stmt = [" ^ String.concat "; " (List.map show_stmt compound_stmt) ^ "] }"


