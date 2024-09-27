{
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


open Parser

let esc_char = function
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | '0' -> '\x00'
        | _ as c -> c

let string_buf = Buffer.create 256

let incr_linenum lexbuf =
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
                pos_lnum = pos.pos_lnum + 1;
                pos_bol = pos.pos_cnum;
        }
}

let digit  = ['0'-'9']
let hex    = digit | ['a'-'f' 'A'-'F']
let letter = ['a'-'z' 'A'-'Z']
let white  = [' ' '\t']
let newline = ['\r' '\n'] | "\r\n"
let esc = '\\' (['n' 't' 'r' '0' '\\' '\'' '"'] | ('x' hex hex ))

rule lexer = parse
      "byte"        { T_byte }
    | "else"        { T_else }
    | "false"       { T_false }
    | "if"          { T_if }
    | "int"         { T_int }
    | "proc"        { T_proc }
    | "reference"   { T_reference }
    | "return"      { T_return }
    | "while"       { T_while }
    | "true"        { T_true}

    | digit+ as num                                     { T_const (int_of_string num) }
    | letter (digit | letter | '_')* as text            { T_id text }
    | '"'                                               { Buffer.clear string_buf; strlex lexbuf; T_string (Buffer.contents string_buf) }
    | '\'' (_ as c) '\''                                { T_char c }
    | "\'\\" (_ as c) '\''                              { T_char (esc_char c) }
    | "\'\\x" (hex hex as code) '\''                    { T_char (Char.chr (int_of_string ("0x" ^ code))) }

    | '='       { T_assign }
    | '+'       { T_plus }
    | '-'       { T_minus }
    | '*'       { T_times }
    | '/'       { T_div }
    | '%'       { T_mod }
    | '!'       { T_not }
    | '&'       { T_and }
    | '|'       { T_or }
    | "=="      { T_eq }
    | "!="      { T_neq }
    | '<'       { T_less }
    | '>'       { T_gr }
    | "<="      { T_leq }
    | ">="      { T_geq }

    | '('       { T_lparen }
    | ')'       { T_rparen }
    | '['       { T_lbrack }
    | ']'       { T_rbrack }
    | '{'       { T_lcbrack }
    | '}'       { T_rcbrack }
    | ','       { T_comma }
    | ':'       { T_colon }
    | ';'       { T_semicol }

    | white+                            { lexer lexbuf }
    | "--" [^'\n' '\r']* newline        { incr_linenum lexbuf; lexer lexbuf }
    | "--" [^'\n' '\r']* eof            { T_eof }

    | newline               { incr_linenum lexbuf; lexer lexbuf }

    | "(*" { comments 0 lexbuf }

    |  eof          { T_eof }
    |  _ as chr     { Printf.eprintf "invalid character: '%c' (ascii: %d) at the character %d of the line %d.\n"
                        chr (Char.code chr) (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol) lexbuf.lex_curr_p.pos_lnum;
                      exit 1; }

and comments level = parse
    | "*)" { 
        if level = 0 then lexer lexbuf
        else comments (level-1) lexbuf
    }
    | "(*" { 
        comments (level+1) lexbuf
    }
    | newline               { incr_linenum lexbuf; comments level lexbuf }
    | _ { comments level lexbuf }
    | eof { Printf.eprintf "comments are not closed\n";  T_eof }

and strlex = parse
        | '"'                           { }
        | "\\x" (hex hex as code)       { Buffer.add_char string_buf (Char.chr (int_of_string ("0x" ^ code))); strlex lexbuf }
        | '\\' (_ as c)                 { Buffer.add_char string_buf (esc_char c); strlex lexbuf }
        | _ as c                        { Buffer.add_char string_buf c; strlex lexbuf }

