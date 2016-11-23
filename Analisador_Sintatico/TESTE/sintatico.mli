
(* The type of tokens. *)

type token = 
  | XOR
  | WRITELN
  | WRITE
  | WHILE
  | VIRGULA
  | VAR
  | USES
  | TO
  | THEN
  | SUBTRACAO
  | STRING
  | SOMA
  | RESTO
  | REAL
  | READLN
  | READ
  | PROGRAMA
  | PONTOVIRGULA
  | OR
  | OF
  | NAO
  | MULTIPLICACAO
  | MENORIGUAL
  | MENOR
  | MAIORIGUAL
  | MAIOR
  | LITSTRING of (string)
  | LITINT of (int)
  | INTEGER
  | IGUAL
  | IF
  | ID of (string)
  | FPAR
  | FOR
  | EOF
  | ENDPROGRAMA
  | ENDMEIOBLOCO
  | ENDBLOCO
  | ELSE
  | DOISPONTOS
  | DO
  | DIVISAO
  | DIFERENTE
  | CHAR
  | CASE
  | BEGIN
  | ATRIB
  | APAR
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val programa: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.programa)
