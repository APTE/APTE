(* File lexer.mll *)
{
open Grammar (* The type token is defined in parser.mli *)

let keyword_table = Hashtbl.create 10

let _ =
  List.iter (fun (kwd,tok) -> Hashtbl.add keyword_table kwd tok)
    [
      "new", NEW;
      "if", IF;
      "then", THEN;
      "else", ELSE;
      "in", IN;
      "out", OUT;
      "fun", FUN;
      "free", FREE;

      "let", LET;
      "and", AND;

      "equivalence" , EQUIVALENCE;
			"secrecy", SECRET;
      "preserve", PRESERVE;

      "length" , LENGTH;
      "constant", CST;
      "arguments", ARGS;
      "tuple", TUPLE
    ]

let newline lexbuf =
      let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <-
        { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                   Lexing.pos_bol = pos.Lexing.pos_cnum }
}
rule token = parse
| '#' [^ '\n']* '\n' { newline lexbuf; token lexbuf }
| "(*" [^ '\n']* "*)" { newline lexbuf; token lexbuf }
|[' ' '\t'] { token lexbuf } (* skip blanks *)
(* Main Configuration *)
| ['\n'	'\r']	{ newline lexbuf;
	token lexbuf }
| '='	 { EQ }
| "<>"	 { NEQ }
| "||"	 { ORECO }
| "&&"	 { ANDECO }
| '/' { SLASH }
| '('	 { LPAR }
| ')'	 { RPAR }
| '['	 { LBRACE }
| ']'	 { RBRACE }
| '|'	 { BARRE }
| '+'	 { PLUS }
| ';'    { PVIR }
| '.'	 { DOT }
| ','    { VIR }
| "proj_" (['0'-'9']* as id_1) "_" (['0'-'9']* as id_2)	{ PROJ(int_of_string id_1,int_of_string id_2) }

| ['A'-'Z' 'a'-'z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id
    {
      try Hashtbl.find keyword_table id
      with Not_found -> STRING(id)
		}
| ([ '0'-'9' ]) +
    {
      try
        INT (int_of_string(Lexing.lexeme lexbuf))
      with Failure _ ->
	let pos = lexbuf.Lexing.lex_curr_p in
	let msg = Printf.sprintf "Line %d : Syntax Error\n" (pos.Lexing.pos_lnum) in

	raise (Failure msg)
    }
| eof { EOF }
| _  {
	let pos = lexbuf.Lexing.lex_curr_p in
	let msg = Printf.sprintf "Line %d : Syntax Error\n" (pos.Lexing.pos_lnum) in

	raise (Failure msg)
}
