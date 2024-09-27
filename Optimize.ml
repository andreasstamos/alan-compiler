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


open Llvm
open Llvm_scalar_opts
open Llvm_passmgr_builder
open Llvm_ipo
open Llvm_vectorize

let llvm_optimize the_module = 
  let fpm = PassManager.create_function the_module in
  let mpm = PassManager.create () in

  (* Function Passes *)
  add_memory_to_register_promotion fpm; 
  add_reassociation fpm;
  add_gvn fpm;
  add_cfg_simplification fpm;
  add_dce fpm;
  add_aggressive_dce fpm;
  add_dead_store_elimination fpm;
  add_tail_call_elimination fpm;
  add_lib_call_simplification fpm;
  add_loop_unroll fpm;
  add_loop_vectorize fpm;
  add_slp_vectorize fpm;

  (* Module Passes *)
  add_internalize mpm ~all_but_main:true;
  add_function_inlining mpm;
  add_global_optimizer mpm;
  add_instruction_combination mpm;
  add_global_dce mpm;
  add_constant_merge mpm;
  add_strip_dead_prototypes mpm;
  add_strip_symbols mpm;

  Llvm.iter_functions (fun func ->
    ignore (PassManager.run_function func fpm)
  ) the_module;

  ignore (PassManager.run_module the_module mpm);

  PassManager.dispose fpm;
  PassManager.dispose mpm;
  
  ()

