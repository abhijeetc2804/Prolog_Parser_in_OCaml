open Lexer
open Parser

    type token =
  | LEFTP
  | RIGHTP
  | LEFT_SQ_BRACKET 
  | RIGHT_SQ_BRACKET
  | SEMICOLON
  | COMMA
  | PERIOD
  | COLON_DASH
  | INT of string
  | BOOL of string
  | FLOAT of string
  | ID of string
  | VARIABLE of string
  | EOF


  
let rec print_expr exp = 
match exp with 
| Parser.PROGRAM lst ->
    Printf.printf "PROGRAM{";
    (* List.iter (fun expr -> print_expr expr) lst; *)
    List.iter (fun expr -> print_expr expr; Printf.printf " ") lst;
    Printf.printf "}"

| Parser.CLAUSE expr ->
    Printf.printf "CLAUSE(";
    print_expr expr;
    Printf.printf ")"

| Parser.FACT expr ->
    Printf.printf "FACT(";
    print_expr expr;
    Printf.printf ")"

| Parser.RULE (head, body) ->
    Printf.printf "RULE(";
    print_expr head;
    Printf.printf ", ";
    print_expr body;
    Printf.printf ")"

| Parser.HEAD expr ->
    Printf.printf "HEAD(";
    print_expr expr;
    Printf.printf ")"

| Parser.BODY lst ->
    Printf.printf "BODY(";
    List.iter (fun expr -> print_expr expr; Printf.printf " ") lst;
    (* List.iter (fun expr -> print_expr expr) lst; *)
    Printf.printf ")"

| Parser.ATOMIC_FORMULA (str, expr) ->
    Printf.printf "ATOMIC_FORMULA(%s, " str;
    print_expr expr;
    Printf.printf ")"

| Parser.VARIABLE str ->
    Printf.printf "VARIABLE(%s)" str

| Parser.ID str ->
    Printf.printf "ID(%s)" str

| Parser.BOOL str ->
    Printf.printf "BOOL(%s)" str

| Parser.FLOAT str ->
    Printf.printf "FLOAT(%s)" str

| Parser.INT str ->
    Printf.printf "INT(%s)" str
    
| Parser.TERM_LIST terms ->
    Printf.printf "TERM_LIST([";
    List.iter (fun term ->
        print_expr term;
        Printf.printf " ") terms;
    Printf.printf "])"

| Parser.INT str ->
    Printf.printf "INT(%s)" str

| Parser.ID str ->
    Printf.printf "ID(%s)" str

| Parser.BOOL str ->
    Printf.printf "BOOL(%s)" str

| Parser.FLOAT str ->
    Printf.printf "FLOAT(%s)" str

| Parser.VARIABLE str ->
    Printf.printf "VARIABLE(%s)" str

| Parser.FUNC (id, term_list) ->
    Printf.printf "ATOMIC_FORMULA(%s, " id;
    print_expr term_list;
    Printf.printf ")"

| Parser.GOAL atoms ->
    Printf.printf "GOAL(";
    List.iter (fun atomic ->
        print_expr atomic;
        Printf.printf " ") atoms;
    Printf.printf ")"

| Parser.LIST expr_list ->
    Printf.printf "LIST([";
    List.iter (fun expr ->
        print_expr expr;
        Printf.printf " ") expr_list;
    Printf.printf "])"
    
| Parser.TUPLE expr_list ->
    Printf.printf "TUPLE(";
    List.iter (fun expr ->
        print_expr expr;
        Printf.printf " ") expr_list;
    Printf.printf ")"

| _ -> Printf.printf "Unknown expression"

  

    (* let rec tokenize_and_print lexbuf =
    let tok = Lexer.token lexbuf in
    match tok with
    | EOF -> print_endline "EOF"
    | LPAREN -> print_endline "LPAREN"; tokenize_and_print lexbuf
    | RPAREN -> print_endline "RPAREN"; tokenize_and_print lexbuf
    | COMMA -> print_endline "COMMA"; tokenize_and_print lexbuf
    | PERIOD -> print_endline "PERIOD"; tokenize_and_print lexbuf
    | ARROW -> print_endline "ARROW"; tokenize_and_print lexbuf
    | INT s -> Printf.printf "INT(%s)\n" s; tokenize_and_print lexbuf
    | BOOL s -> Printf.printf "BOOL(%s)\n" s; tokenize_and_print lexbuf
    | FLOAT s -> Printf.printf "FLOAT(%s)\n" s; tokenize_and_print lexbuf
    | ID s -> Printf.printf "ID(%s)\n" s; tokenize_and_print lexbuf
    | VARIABLE s -> Printf.printf "VARIABLE(%s)\n" s; tokenize_and_print lexbuf
    

  let () =
  let lexbuf = Lexing.from_channel stdin in
  tokenize_and_print lexbuf *)

let () =
    let lexbuf = Lexing.from_channel stdin in

    (* Parse and print the parse tree *)
    let parse_tree = Parser.program Lexer.token lexbuf in
    Printf.printf "Parse Tree: ";
    print_expr parse_tree;
    print_endline "";
