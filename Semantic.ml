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

module StringMap = Map.Make(String)

(* A Map from string to ttype is used as symbol table.
   We do not need special infrastucture for scopes, because we can keep the old symbol table
   before the scope begins. This is possible because the Map is a functional data structure.
*)

(* The Result type is used for handling failures. We try to use it as a monad for conciseness and elegance.
   What happens is that when one semantic check fails the checks stop and a Result.Error is returned totally,
   otherwise, when one succeeds we proceed to the next one. This behaviour is wrapped by the Result.bind (monadic bind)
*)


let (let*) = Result.bind

let fold_result f lst init =
        let rec aux acc = function
                | [] -> Result.Ok acc
                | x :: xs -> 
                                let* acc' = f acc x in
                                aux acc' xs
                                (* above is monad syntax. equivalent:  Result.bind (f acc x) (fun acc' -> aux acc' xs)
                                *)
        in aux init lst

let find_duplicates lst =
  let lst_sort = List.sort compare lst in
  let rec aux acc prev_dup = function
    | x1::((x2::_) as xs) when x1 = x2 -> aux acc true xs
    | x::xs when prev_dup -> aux (x::acc) false xs
    | _::xs -> aux acc false xs
    | [] -> List.rev acc
  in aux [] false lst_sort


let (>=>) g f x = Result.bind (g x) f (* also known as kleisli composition *)


let rec semFunc (ast: Ast.funcDef) env : (unit, string) result =
  let checkParamTypes () = fold_result (fun _ p -> match p.ttype with
                  | Int_t | Byte_t | Ref Int_t | Ref Byte_t | Ref (Array { ttype = Int_t; _ } ) | Ref (Array { ttype = Byte_t; _ } ) ->
                                  Result.Ok ()
                  | _ -> Result.Error ("Function " ^ (ast.id) ^ " is defined with formal paramater \"" ^ (p.id) ^ "\" with invalid type: " ^ ( ShowAst.show_ttype (p.ttype)))) ast.params ()
  in
  let checkRType () = match ast.rtype with
                  | Proc_t | Byte_t | Int_t -> Result.Ok ()
                  | _ -> Result.Error ("Function " ^ (ast.id) ^ " is defined with invalid return type: " ^ ( ShowAst.show_ttype (ast.rtype)))
  in
  let checkIdentifiers () =
    let identifiers = (List.map (function | LocalVar {id} | LocalFun {id} -> id) ast.localdef)
      @ (List.map (fun (v:varDef) -> v.id) ast.params)
    in
    let duplicates = find_duplicates identifiers in
    if List.is_empty duplicates then Result.Ok () else
       Result.Error ("Function " ^ ast.id ^ " has many definitions for the " ^ 
         (if List.length duplicates = 1 then "identifier: " else "identifiers: ") ^
         (String.concat ", " duplicates))
  in
  (* add function parameters to env *)
  let addParams env' =
    List.fold_left (fun env'' (param: varDef) -> StringMap.add param.id param.ttype env'') env' ast.params
  in
  let checkLocalDefs env' = fold_result (fun env'' localdef -> match localdef with
                  | LocalVar vdef -> (match vdef.ttype with
                          | Int_t | Byte_t
                          | (Array { ttype = Int_t; length = Some _ } ) | (Array { ttype = Byte_t; length = Some _ } ) ->
                            Result.Ok (env'' |> StringMap.add vdef.id vdef.ttype)
                          | _ -> Result.Error ("Function " ^ (ast.id) ^ " has a local variable " ^ vdef.id ^ "with bad type: " ^ ( ShowAst.show_ttype (vdef.ttype)))
                  )
                  | LocalFun fdef -> Result.Ok (env'' |> StringMap.add fdef.id (Func ((List.map (fun p -> p.ttype) fdef.params), fdef.rtype)))) ast.localdef env'
  in
  let checkLocalFuns env' = fold_result (fun () localdef -> match localdef with
                  | LocalFun fdef -> semFunc fdef env'
                  | _ -> Result.Ok ()
  ) ast.localdef ()
  in
  let* _ = checkParamTypes () in
  let* _ = checkRType () in
  let* _ = checkIdentifiers () in
  let env_with_params = addParams env in (* add parameters to env *)
  let* env' = checkLocalDefs env_with_params in
  let* _ = checkLocalFuns env' in
  let* _ = checkReturn env' ast.rtype (CompoundStmt ast.compound_stmt) in
  let* _ = semStmt env' (CompoundStmt ast.compound_stmt) in
  Result.Ok ()

and noRef t = match t with
        | Ref t -> noRef t
        | _ -> t

and checkEq t1 t2 = match (noRef t1, noRef t2) with
  | Array { ttype = t1'; _ }, Array { ttype = t2'; _ } -> checkEq t1' t2'
  | (t1', t2') -> if t1' = t2' then Result.Ok () else Result.Error ("Type error " ^ (ShowAst.show_ttype t1') ^ " != " ^ (ShowAst.show_ttype t2'))

and checkNum t = match noRef t with
        | Int_t | Byte_t -> Result.Ok ()
        | t' -> Result.Error ("Expected number type (Int or Byte) but found: " ^ (ShowAst.show_ttype t'))

and checkReturn env t (stmt: Ast.stmt) =
        match stmt with
                | CompoundStmt stmts -> fold_result (fun _ -> checkReturn env t) stmts ()
                | IfThenElse { tthen = tthen; eelse = None; _ } -> checkReturn env t tthen
                | IfThenElse { tthen = tthen; eelse = Some eelse; _ } -> 
                                let* x = checkReturn env t tthen in
                                checkReturn env t eelse
                | While { stmt = stmt; _ } -> checkReturn env t stmt
                | Return None -> checkEq t Proc_t
                | Return Some e ->
                                let* te = semExpr env e
                                in checkEq te t
                | _ -> Result.Ok ()

and semExpr env expr =
  match expr with
  | ConstInt _ -> Result.Ok Int_t
  | ConstChar _ -> Result.Ok Byte_t
  | LValue_e lvalue -> semLvalue lvalue env
  | FuncCall_e func_call -> semFuncCall func_call env
  | UnMinus e ->
    let* te = semExpr env e in
    let* _ = checkNum te in
    Result.Ok te
  | Plus (e1, e2) | Minus (e1, e2) | Mul (e1, e2) | Div (e1, e2) | Mod (e1, e2) ->
    let* te1 = semExpr env e1 in
    let* te2 = semExpr env e2 in
    let* _ = checkNum te1 in
    let* _ = checkNum te2 in
    let* _ = checkEq te1 te2 in
    Result.Ok te1

and semStmt env (stmt: Ast.stmt) : (unit, string) result =
        match stmt with
                | CompoundStmt stmts -> fold_result (fun _ -> semStmt env) stmts ()
                | Assign { lvalue = lvalue; expr = expr } ->
                                let* t1 = semLvalue lvalue env in
                                let* t2 = semExpr env expr in
                                let* _ = (fun _ -> match t1 with
                                        | Array _ -> Result.Error ("Assignment on variable with array type: " ^ ShowAst.show_ttype t1 ^ " .")
                                        | _ -> Result.Ok () ) () in
                                checkEq t1 t2
                | FuncCall_s funCall ->
                                let* t = semFuncCall funCall env in
                                checkEq t Proc_t
                | IfThenElse { cond = cond; tthen = tthen; eelse = None } -> 
                                let* _ = semCond cond env in
                                let* _ = semStmt env tthen in
                                Result.Ok ()
                | IfThenElse { cond = cond; tthen = tthen; eelse = Some eelse } ->
                                let* _ = semCond cond env in
                                let* _ = semStmt env tthen in
                                let* _ = semStmt env eelse in
                                Result.Ok ()
                | While { cond = cond; stmt = stmt } -> 
                                let* _ = semCond cond env in
                                let* _ = semStmt env stmt in
                                Result.Ok ()
                | Return _ -> Result.Ok ()  
                | Null -> Result.Ok ()

and semLvalue (lvalue: Ast.lvalue) env =
        match lvalue with
                | Var id ->
                                (match env |> StringMap.find_opt id with
                                        | Some t -> Result.Ok t
                                        | None -> Result.Error ("Use of undeclared variable " ^ id ^ " .")
                                )
                | VarIdx (id,idx) ->
                                let* t = semLvalue (Var id) env in
                                let* t_idx = semExpr env idx in
                                let* _ = checkEq t_idx Int_t in
                                (match noRef t with (* TODO *)
                                        | Array { ttype; _ } -> Result.Ok ttype
                                        | _ -> Result.Error ("Indexing non-array type: " ^ ShowAst.show_ttype t)         )
                | StrLiteral _ -> Result.Ok (Array { ttype = Byte_t; length = None } )


and semFuncCall func_call env =
  match StringMap.find_opt func_call.id env with
  | Some (Func (param_types, return_type)) ->
    if List.length param_types = List.length func_call.params then
      let* _ = fold_result (fun _ (param_type, param) ->
                let* t = semExpr env param in
                checkEq t param_type)
        (List.combine param_types func_call.params) () in
      Result.Ok return_type
    else
      Result.Error ("Function " ^ func_call.id ^ " called with wrong number of arguments.")
  | Some _ -> Result.Error ("Function " ^ func_call.id ^ " is not a function.")
  | None -> Result.Error ("Call to undeclared function " ^ func_call.id)

and semCond cond env =
  match cond with
  | True_c | False_c -> Result.Ok ()
  | Not c -> semCond c env
  | Eq (e1, e2) | Neq (e1, e2) | Lt (e1, e2) | Gt (e1, e2) | Leq (e1, e2) | Geq (e1, e2) ->
    let* t1 = semExpr env e1 in
    let* t2 = semExpr env e2 in
    let* _ = checkEq t1 t2 in
    Result.Ok ()
  | And (c1, c2) | Or (c1, c2) -> 
                  let* _ = semCond c1 env in
                  let* _ = semCond c2 env in
                  Result.Ok ()

let semDecl (decl: Ast.funcDecl) = 
  let checkParamTypes () = fold_result (fun _ p -> match p with
                        | Int_t | Byte_t | Ref Int_t | Ref Byte_t | Ref (Array { ttype = Int_t; _ } ) | Ref (Array { ttype = Byte_t; _ } ) ->
                                        Result.Ok ()
                        | _ -> Result.Error ("Function " ^ decl.id ^ " is declared with formal paramater with invalid type: " ^ ( ShowAst.show_ttype p))) decl.params ()
  in
  let checkRType () = match decl.rtype with
                        | Proc_t | Byte_t | Int_t -> Result.Ok ()
                        | _ -> Result.Error ("Function " ^ decl.id ^ " is declared with invalid return type: " ^ ( ShowAst.show_ttype (decl.rtype)))
  in
  let* _ = checkParamTypes () in
  let* _ = checkRType () in
  Result.Ok ()

let semDecls (decls: Ast.funcDecl list) env =
  
  let identifiers = List.map (fun (f:funcDecl) -> f.id) decls in
  
  let checkMultipleDecls () =
    let duplicates = find_duplicates identifiers in
    if List.is_empty duplicates then Result.Ok () else
       Result.Error ("Found many declarations for the " ^ 
         (if List.length duplicates = 1 then "identifier: " else "identifiers: ") ^
         (String.concat ", " duplicates))
  in
  let* _ = fold_result (fun _ decl -> semDecl decl) decls () in
  let* _ = checkMultipleDecls () in
  let env = List.fold_left (fun env (decl:funcDecl) -> (env |> StringMap.add decl.id (Func (decl.params, decl.rtype)))) env decls
  in Result.Ok env

let semProgram (func: Ast.funcDef) env =
        (* if something is wrong here it will be caught by semFunc. the only reason for this wrapper is because
           semFunc expects the function to be already in env. *)
        let env = env |> StringMap.add func.id (Func ((List.map (fun p -> p.ttype) func.params), func.rtype))
        in semFunc func env

