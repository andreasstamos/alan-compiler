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


open Codegen
open Llvm
open Optimize
open Sys

let (let*) = Result.bind

let rec foldM f acc = function
  | [] -> Result.Ok acc
  | x::xs ->
    let* acc' = f acc x in
    foldM f acc' xs

(* Function to format and print the position of an error *)
let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  Printf.printf "Syntax error at line %d, column %d\n" line col

let process_decls fn sem_env =
  let ic = open_in fn in
  let lexbuf = Lexing.from_channel ic in
  let decls = (try
       let decls = Parser.declarations Lexer.lexer lexbuf in
       close_in ic;
       decls
     with Parsing.Parse_error ->
       print_position lexbuf;
       print_endline (" in declarations file: " ^ fn);
       exit 1;
    ) in

  let sem_env_m = Semantic.semDecls decls sem_env in

  let sem_env = (match sem_env_m with
     | Result.Ok env -> env
     | Result.Error err ->
       print_endline err;
       exit 1;
    ) in
  (decls, sem_env)

  let usage () =
    Printf.printf "Usage: %s [options] [filename]\n" (Array.get Sys.argv 0);
    Printf.printf "Options:\n";
    Printf.printf "  -O           Enable optimizations\n";
    Printf.printf "  -o <file>    Specify output executable filename (default: a.out)\n";
    Printf.printf "  -f           Read input from stdin and output to stdout\n";
    Printf.printf "  -i           Read input from stdin and print LLVM IR to stdout\n";
    Printf.printf "  -h, --help   Show this help message\n"
  
  let parse_args () =
    let input_file = ref None in
    let output_file = ref "a.out" in
    let optimize = ref false in
    let input_from_stdin = ref false in
    let print_ir = ref false in
    let print_exec = ref false in

    let rec parse = function
      | [] -> ()
      | "-O" :: rest ->
        optimize := true;
        parse rest
      | "-o" :: filename :: rest ->
        output_file := filename;
        parse rest
      | "-f" :: rest ->
        input_from_stdin := true;
        print_exec := true;
        parse rest
      | "-i" :: rest ->
        input_from_stdin := true;
        print_ir := true;
        parse rest
      | "-h" :: _ | "--help" :: _ ->
        usage ();
        exit 0
      | filename :: rest when Option.is_none !input_file ->
        input_file := Some filename;
        parse rest
      | _ ->
        Printf.printf "Unknown argument\n";
        usage ();
        exit 1
    in
  parse (Array.to_list Sys.argv |> List.tl); 
  (!input_file, !output_file, !optimize, !input_from_stdin, !print_ir, !print_exec)

let () =
  let (input_file, output_file, optimize_flag, input_from_stdin, print_ir, print_exec) = parse_args () in

  let sem_env = Semantic.StringMap.empty in
  let (decls, sem_env) = process_decls "global.aland" sem_env in

  let lexbuf =
    if input_from_stdin then Lexing.from_channel stdin
    else match input_file with
         | Some filename -> Lexing.from_channel (open_in filename)
         | None ->
           Printf.printf "Error: Input file must be specified unless -f or -i is used.\n";
           usage ();
           exit 1
  in

  let prog = (try
       Parser.program Lexer.lexer lexbuf
     with Parsing.Parse_error ->
       print_position lexbuf;
       print_endline " in program input";
       exit 1
    ) in
  
  let sem_status_m = Semantic.semProgram prog sem_env in
  
  (match sem_status_m with
   | Result.Ok _ -> ()
   | Result.Error err ->
     print_endline err;
     exit 1;
  );
  
  let codegen_env = List.fold_left codegen_func_decl StringMap.empty decls in
  codegen_program prog codegen_env;

  (* Verify the module *)
  Llvm_analysis.assert_valid_module the_module;

  if optimize_flag then llvm_optimize the_module;

  (* Print the LLVM IR *)
  if print_ir then (
    print_string (string_of_llmodule the_module)
  ) else (
    let tmp_ir_file = Filename.temp_file "output" ".ll" in
    let ir_channel = open_out tmp_ir_file in
    output_string ir_channel (string_of_llmodule the_module);
    close_out ir_channel;

    let clang_command = Printf.sprintf "clang-15 %s -o %s %s lib-alan/*" (if optimize_flag then "-O3" else "") output_file tmp_ir_file in
    let clang_result = Sys.command clang_command in

    if clang_result <> 0 then (
      Printf.printf "Error: Failed to generate executable\n";
      exit 1
    );

    if print_exec then
      ignore (Sys.command ("cat " ^ output_file));

    Sys.remove tmp_ir_file;
  );

  exit 0
