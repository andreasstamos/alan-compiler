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


(* Import the LLVM modules *)
open Llvm
open Llvm_target

open Ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type symbol = { llvalue: llvalue; ttype: ttype }


(* Initialize the LLVM context and module *)
let context = global_context ()
let the_module = create_module context "alan_module"

(* Retrieve the default target triple for the host system *)
let target_triple = Target.default_triple ()

(* Set the target triple for the module *)
let () = Llvm.set_target_triple target_triple the_module

(* Define commonly used LLVM types *)
let i32_type = i32_type context
let i8_type = i8_type context
let i1_type = i1_type context
let void_type = void_type context

(* Type translation from ttype to LLVM types *)
let rec llvm_type_of_ttype ttype =
  match ttype with
  | Proc_t -> void_type
  | Int_t -> i32_type
  | Byte_t -> i8_type
  | Array { ttype = elem_type; length = None } | Ref (Array {ttype = elem_type;} ) ->
      let llvm_elem_type = llvm_type_of_ttype elem_type in
      pointer_type llvm_elem_type
  | Array { ttype = elem_type; length = Some length } ->
      let llvm_elem_type = llvm_type_of_ttype elem_type in
      array_type llvm_elem_type length
  | Func2 { param_types = param_types; rtype = rtype }->
      let llvm_param_types = Array.of_list (List.map llvm_type_of_ttype param_types) in
      let llvm_ret_type = llvm_type_of_ttype rtype in
      function_type llvm_ret_type llvm_param_types
  | Ref t -> pointer_type (llvm_type_of_ttype t)

(* Code generation for expressions *)
let rec codegen_expr builder env expr =
  match expr with
  | ConstInt n ->
      const_int i32_type n
  | ConstChar c ->
      const_int i8_type (Char.code c)
  | LValue_e lval ->
      let {llvalue = ptr; ttype = ptr_deref_ttype} = codegen_lvalue builder env lval in
      (* dereference to get the actual value if a reference*)
      if match ptr_deref_ttype with Ref _ -> true | _ -> false then
        build_load2 (llvm_type_of_ttype (match ptr_deref_ttype with Ref t -> t | _ -> ptr_deref_ttype)) ptr "" builder
      else
        build_load2 (llvm_type_of_ttype ptr_deref_ttype) ptr "" builder
  | FuncCall_e func_call ->
      codegen_func_call builder env func_call
  | UnMinus e ->
      let e_val = codegen_expr builder env e in
      build_neg e_val "negtmp" builder
  | Plus (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_add v1 v2 "addtmp" builder
  | Minus (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_sub v1 v2 "subtmp" builder
  | Mul (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_mul v1 v2 "multmp" builder
  | Div (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_sdiv v1 v2 "divtmp" builder
  | Mod (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_srem v1 v2 "modtmp" builder

(* Code generation for lvalues *)
and codegen_lvalue builder env lval =
  match lval with
  | Var name -> StringMap.find name env
  | VarIdx (name, idx_expr) ->
         let {llvalue = var; ttype = var_type; } = StringMap.find name env in
         let elem_type = (match var_type with 
           | Array { ttype = elem_type } -> elem_type
           | Ref Array { ttype = elem_type } -> elem_type )
         in
         let idx = codegen_expr builder env idx_expr in
         let llvalue = build_in_bounds_gep2 (llvm_type_of_ttype elem_type) var [| idx |] (name ^ "_idx_ptr") builder in
         { llvalue = llvalue; ttype = elem_type }
  | StrLiteral s -> 
         let llvalue = build_global_stringptr s "str" builder in
         { llvalue = llvalue; ttype = Array { ttype = Byte_t; length = None; } }


(* Code generation for function calls *)
and codegen_func_call builder env func_call =
  let {llvalue = callee; ttype = ((Func2 {param_types; captured}) as callee_type)} = StringMap.find func_call.id env in
  let params_captured = List.map (fun v -> LValue_e (Var v)) captured in
  let params = List.map (function
          | (Ref _, LValue_e lval) -> (codegen_lvalue builder env lval).llvalue
          | (_, expr) -> codegen_expr builder env expr
          ) (List.combine param_types (func_call.params @ params_captured)) in
  let params_array = Array.of_list params in
  build_call2 (llvm_type_of_ttype callee_type) callee params_array "" builder

(* Code generation for statements *)
let rec codegen_stmt builder env stmt =
  match stmt with
  | CompoundStmt stmts ->
      List.iter (codegen_stmt builder env) stmts
  | Assign { lvalue; expr } ->
      let expr_val = codegen_expr builder env expr in
      let {llvalue = ptr; ttype = ptr_deref_ttype} = codegen_lvalue builder env lvalue in
      (* ignore (build_store2 (llvm_type_of_ttype ptr_deref_ttype) expr_val ptr builder) *)
      (* TODO: In the future, we need to change to the above as the below is deprecated.
         But in llvm 15 bindings for some reason build_store2 is missing *)
      
         (* For references, store the value at the dereferenced pointer *)
      let value_to_store = match ptr_deref_ttype with
        | Ref t -> expr_val  (* expr_val already has the loaded value *)
        | _ -> expr_val in
      ignore (build_store value_to_store ptr builder)
  | FuncCall_s func_call ->
      ignore (codegen_func_call builder env func_call)
  | IfThenElse { cond; tthen; eelse } ->
      codegen_if_then_else builder env cond tthen eelse
  | While { cond; stmt } ->
      codegen_while builder env cond stmt
  | Return expr_opt ->
      (match expr_opt with
      | Some expr ->
          let ret_val = codegen_expr builder env expr in
          ignore (build_ret ret_val builder)
      | None ->
          ignore (build_ret_void builder))
  | Null ->
      ()

(* Code generation for conditions *)
and codegen_cond builder env cond =
  match cond with
  | True_c ->
      const_int i1_type 1
  | False_c ->
      const_int i1_type 0
  | Not c ->
      let c_val = codegen_cond builder env c in
      build_not c_val "nottmp" builder
  | Eq (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_icmp Icmp.Eq v1 v2 "eqtmp" builder
  | Neq (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_icmp Icmp.Ne v1 v2 "netmp" builder
  | Lt (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_icmp Icmp.Slt v1 v2 "lttmp" builder
  | Gt (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_icmp Icmp.Sgt v1 v2 "gttmp" builder
  | Leq (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_icmp Icmp.Sle v1 v2 "leqtmp" builder
  | Geq (e1, e2) ->
      let v1 = codegen_expr builder env e1 in
      let v2 = codegen_expr builder env e2 in
      build_icmp Icmp.Sge v1 v2 "geqtmp" builder
  | And (c1, c2) ->
      let v1 = codegen_cond builder env c1 in
      
      let current_block = insertion_block builder in
      let the_function = block_parent current_block in
      let then_bb = append_block context "and_then" the_function in
      let end_bb = append_block context "and_end" the_function in
      
      (* If v1 is true, only then evaluate c2. *)
      ignore (build_cond_br v1 then_bb end_bb builder);
      
      (* evalute c2 *)
      position_at_end then_bb builder;
      let v2 = codegen_cond builder env c2 in
      ignore (build_br end_bb builder);

      (* end bb *)
      position_at_end end_bb builder;
      
      build_phi [(const_int i1_type 0, current_block); (v2, then_bb)] "andtmp" builder
  | Or (c1, c2) ->
      let v1 = codegen_cond builder env c1 in
      
      let current_block = insertion_block builder in
      let the_function = block_parent current_block in
      let then_bb = append_block context "or_then" the_function in
      let end_bb = append_block context "or_end" the_function in
      
      (* If v1 is true, do not evaluate c2. *)
      ignore (build_cond_br v1 end_bb then_bb builder);
      
      (* evalute c2 *)
      position_at_end then_bb builder;
      let v2 = codegen_cond builder env c2 in
      ignore (build_br end_bb builder);

      (* end bb *)
      position_at_end end_bb builder;
      
      build_phi [(const_int i1_type 1, current_block); (v2, then_bb)] "ortmp" builder


(* Code generation for if-then-else statements *)
and codegen_if_then_else builder env cond tthen eelse_opt =
  let cond_val = codegen_cond builder env cond in
  let start_bb = insertion_block builder in
  let the_function = block_parent start_bb in

  let then_bb = append_block context "then" the_function in
  let else_bb = append_block context "else" the_function in
  let merge_bb = append_block context "merge" the_function in

  ignore (build_cond_br cond_val then_bb else_bb builder);

  (* Then block *)
  position_at_end then_bb builder;
  codegen_stmt builder env tthen;
  if block_terminator (insertion_block builder) = None then
    ignore (build_br merge_bb builder);

  (* Else block *)
  position_at_end else_bb builder;
  (match eelse_opt with
  | Some eelse_stmt ->
      codegen_stmt builder env eelse_stmt
  | None -> ());
  if block_terminator (insertion_block builder) = None then
    ignore (build_br merge_bb builder);

  (* Merge block *)
  position_at_end merge_bb builder;
  ()

(* Code generation for while loops *)
and codegen_while builder env cond body_stmt =
  let the_function = block_parent (insertion_block builder) in

  let loop_bb = append_block context "loop" the_function in
  let after_bb = append_block context "afterloop" the_function in

  (* Jump to loop condition *)
  ignore (build_br loop_bb builder);

  (* Loop condition block *)
  position_at_end loop_bb builder;
  let cond_val = codegen_cond builder env cond in

  let body_bb = append_block context "loopbody" the_function in
  ignore (build_cond_br cond_val body_bb after_bb builder);

  (* Loop body block *)
  position_at_end body_bb builder;
  codegen_stmt builder env body_stmt;
  ignore (build_br loop_bb builder);

  (* After loop *)
  position_at_end after_bb builder;
  ()

(* Analyze the function body to find captured variables *)
let rec find_captured_vars (func_def:funcDef) env =
  let env = List.fold_left (fun env (v:varDef) -> StringSet.add v.id env) env func_def.params in
  let env = List.fold_left (fun env (v:localDef) ->
          let identifier = match v with
            | LocalVar { id; } -> id
            | LocalFun { id; } -> id
          in
          StringSet.add identifier env
  ) env func_def.localdef in

  let var_vars name = if not (StringSet.mem name env) then [name] else [] in

  let rec expr_vars expr =
    match expr with
    | ConstInt _ | ConstChar _ -> []
    | LValue_e (Var name) -> var_vars name
    | LValue_e (VarIdx (name, idx_expr)) -> (var_vars name @ expr_vars idx_expr)
    | LValue_e (StrLiteral _) -> []
    | FuncCall_e func_call -> List.concat_map expr_vars func_call.params
    | UnMinus e -> expr_vars e
    | Plus (e1, e2) | Minus (e1, e2) | Mul (e1, e2)
    | Div (e1, e2) | Mod (e1, e2) ->
        (expr_vars e1) @ (expr_vars e2)

  and cond_vars cond =
    match cond with
    | True_c | False_c -> []
    | Not c -> cond_vars c
    | Eq (e1, e2) | Neq (e1, e2) | Lt (e1, e2)
    | Gt (e1, e2) | Leq (e1, e2) | Geq (e1, e2) ->
        (expr_vars e1) @ (expr_vars e2)
    | And (c1, c2) | Or (c1, c2) ->
        (cond_vars c1) @ (cond_vars c2)

  and stmt_vars stmt =
    match stmt with
    | CompoundStmt stmts -> List.concat_map stmt_vars stmts
    | Assign { lvalue; expr } ->
        (match lvalue with
        | Var name -> var_vars name
        | VarIdx (name, idx_expr) -> (var_vars name) @ (expr_vars idx_expr)
        | StrLiteral _ -> []) @ (expr_vars expr)
    | FuncCall_s func_call ->
        List.concat_map expr_vars func_call.params
    | IfThenElse { cond; tthen; eelse } ->
        (cond_vars cond) @ (stmt_vars tthen) @
        (match eelse with
        | Some eelse_stmt -> stmt_vars eelse_stmt
        | None -> [])
    | While { cond; stmt } -> (cond_vars cond) @ (stmt_vars stmt)
    | Return expr_opt ->
        (match expr_opt with
        | Some expr -> expr_vars expr
        | None -> [])
    | Null -> []
  in

  (* Analyze local definitions *)
  let localdef_captured = List.concat_map (function
    | LocalVar _ -> []
    | LocalFun f_def -> find_captured_vars f_def env
  ) func_def.localdef in

  (* Analyze function body *)
  let funcdef_captured = List.concat_map stmt_vars func_def.compound_stmt in
  let captured = localdef_captured @ funcdef_captured in

  List.sort_uniq compare captured


(* Code generation for function definitions *)
let rec codegen_func_def env (func_def: funcDef) =
  (* Get function from environment *)
  let {llvalue = function_llvalue; ttype = Func2 function_ttype} = StringMap.find func_def.id env in

  (* Create a new builder for this function *)
  let builder = builder_at_end context (entry_block function_llvalue) in
  
  (* Add parameters to the environment and code generation for them *)
  let (_, env) = List.fold_left (fun (i,env) (param_name, param_ttype) ->
    let param_llvalue = param function_llvalue i in
    let param_ptr = (match param_ttype with
        | (Ref _ ) -> param_llvalue
        | _ ->
            let param_alloca = build_alloca (llvm_type_of_ttype param_ttype) param_name builder in
            (* ignore (build_store2 (llvm_type_of_ttype param_ttype) param_llvalue param_alloca builder); *)
            (* TODO: In the future, we need to change to the above as the below is deprecated.
            But in llvm 15 bindings for some reason build_store2 is missing *)
            ignore (build_store param_llvalue param_alloca builder);
            param_alloca)
     in (i+1, StringMap.add param_name {llvalue = param_ptr; ttype = param_ttype } env)
  ) (0, env) (List.combine ((List.map (fun (p:varDef) -> p.id) func_def.params) @ function_ttype.captured) function_ttype.param_types) in

  let rec noRef t = match t with
        | Ref t -> noRef t
        | _ -> t in

  (* Add local variable definitions to the environment *)
  let env = List.fold_left (fun env local_def -> match local_def with
          | LocalVar var_def -> 
            let var_name = var_def.id in
            let var_type = var_def.ttype in
            let var_alloca = (match var_type with
              | Array { ttype = elem_type; length = Some len } ->
                  let llvm_elem_type = llvm_type_of_ttype elem_type in
                  let array_length = const_int i32_type len in
                  build_array_alloca llvm_elem_type array_length var_def.id builder
              | _ ->
                  let llvm_var_type = llvm_type_of_ttype var_type in
                  build_alloca llvm_var_type var_name builder
              ) in
            StringMap.add var_name {llvalue = var_alloca; ttype = var_type } env
          | _ -> env
  ) env func_def.localdef in


  (* Add local function definitions to the environment *)
  let env = List.fold_left (fun env local_def -> match local_def with
          | LocalFun func_def ->
             let captured_vars = find_captured_vars func_def StringSet.empty in
             let captured_vars_types = List.map (fun v -> Ref (noRef (StringMap.find v env).ttype)) captured_vars in
 
             let func_name = func_def.id in
             let param_types = (List.map (fun (v:varDef) -> v.ttype) func_def.params) @ captured_vars_types in
             let func_type = Func2 {param_types = param_types; rtype = func_def.rtype; captured = captured_vars} in
             let func_type_llvm = llvm_type_of_ttype func_type in
             let function_llvalue = define_function func_name func_type_llvm the_module in

             StringMap.add func_def.id {
                     llvalue = function_llvalue;
                     ttype = func_type;
             } env
          | _ -> env
  ) env func_def.localdef in

  (* Generate code for local function definitions *)
  List.iter (function 
    | LocalVar var_def -> ()
    | LocalFun func_def ->
      (* Handle local function definitions recursively *)
      codegen_func_def env func_def
  ) func_def.localdef;

  (* Generate code for the function body *)
  List.iter (fun stmt ->
    codegen_stmt builder env stmt
  ) func_def.compound_stmt;

  (* Ensure the function has a return instruction *)
  if block_terminator (insertion_block builder) = None then
    match func_def.rtype with
    | Proc_t ->
        ignore (build_ret_void builder);
    | _ -> ignore (build_unreachable builder);

  (* Verify the function *)
  Llvm_analysis.assert_valid_function function_llvalue;
  ()

let codegen_func_decl env (func_def: funcDecl) =
  let func_name = func_def.id in
  let param_types = func_def.params in
  let func_type = Func2 {param_types = param_types; rtype = func_def.rtype; captured = []} in
  let func_type_llvm = llvm_type_of_ttype func_type in
  let function_llvalue = declare_function func_name func_type_llvm the_module in
  
  StringMap.add func_def.id {
       llvalue = function_llvalue;
       ttype = func_type;
  } env
  

(* Generate code for the entire program *)
let codegen_program (func_def: funcDef) env =
  let main_func_def = {
    id = "main";
    params = [];
    rtype = Int_t;
    localdef = [LocalFun func_def];
    compound_stmt = [
      FuncCall_s { id = func_def.id; params = [] };
      Return (Some (ConstInt 0));
    ];
  }
  in
  let func_type = Func2 {param_types = []; rtype = Int_t; captured = []} in
  let func_type_llvm = llvm_type_of_ttype func_type in
  let function_llvalue = define_function "main" func_type_llvm the_module in
 
  let env = StringMap.add "main" {
          llvalue = function_llvalue;
          ttype = func_type;
  } env
  in
  codegen_func_def env main_func_def

