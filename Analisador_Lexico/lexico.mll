{
  open Lexing
  open Printf

  let incr_num_linha lexbuf = 
    let pos = lexbuf.lex_curr_p in
     lexbuf.lex_curr_p <- { pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = pos.pos_cnum;
     }


  let caracter_erro lexbuf c =
    let pos = lexbuf.lex_curr_p in
    let lin = pos.pos_lnum
    and col = pos.pos_cnum - pos.pos_bol - 1 in
    sprintf "%d-%d: caracter desconhecido %c" lin col c

  let msg_erro lexbuf oque =
    let pos = lexbuf.lex_curr_p in
    let lin = pos.pos_lnum in
    sprintf "%s nao fechado: linha:%d" oque lin  

type tokens = PROGRAMA
            | USES
            | VAR
            | INTEGER
            | CHAR
            | STRING    (*NO ANTERIOR NÃO TINHA!!!*)
            | REAL
            | BEGIN
            | ENDPROGRAMA
            | ENDBLOCO
            | ENDMEIOBLOCO (*NAO TINHA*)
            | PONTOVIRGULA
            | VIRGULA
            | APAR
            | FPAR
            | ATRIB
            | DOISPONTOS
            | NAO 
            | AND 
            | OR
            | XOR
            | MAIOR
            | MENOR
            | MAIORIGUAL
            | MENORIGUAL
            | IGUAL
            | DIFERENTE
            | SOMA
            | SUBTRACAO
            | DIVISAO
            | MULTIPLICACAO
            | RESTO
            | IF
            | CASE
            | OF
            | THEN
            | ELSE
            | WHILE
            | DO
            | FOR
            | TO 
            | WRITE
            | WRITELN
            | READ
            | READLN
            | LITINT of int
            | LITSTRING of string
            | LITFLOAT of float
            | LITCHAR of string
            | ID of string
            | EOF
            (*
            | LITCHAR of char
            | BYTE
            *)
            
}

let digito = ['0' - '9']
let inteiro = digito+
let flutuante = digito+ '.' digito+

let letra = ['a' - 'z' 'A' - 'Z']
let identificador = letra ( letra | digito | '_')*

let brancos = [' ' '\t']+
let novalinha = '\r' | '\n' | "\r\n"
let comentario = "//" [^ '\r' '\n' ]*

rule token = parse
  brancos    { token lexbuf }
| novalinha  { incr_num_linha lexbuf; token lexbuf }
| comentario { token lexbuf }
| "(*"       { comentario_bloco 1 0  lexbuf }
| "{"        { comentario_bloco 0 1 lexbuf }
| '('        { APAR }
| ')'        { FPAR }
| ":="       { ATRIB }
| ':'        { DOISPONTOS }
| '>'        { MAIOR }
| '<'        { MENOR }
| ">="       { MAIORIGUAL }
| "<="       { MENORIGUAL }
| '='        { IGUAL }
| "not"      { NAO }
| '+'        { SOMA }
| '-'        { SUBTRACAO }
| '*'        { MULTIPLICACAO }
| '/'        { DIVISAO }
| "div"      { DIVISAO }
| "mod"      { RESTO }
| "and"      { AND }
| "xor"      { XOR }
| "or"       { OR }
| "<>"       { DIFERENTE }
| ';'        { PONTOVIRGULA }
| ","        { VIRGULA }
| ":"        { DOISPONTOS }
| "program"  { PROGRAMA }
| "var"      { VAR }
| "uses"     { USES }
| "begin"    { BEGIN }
| "end."     { ENDPROGRAMA }
| "end;"     { ENDBLOCO }
| "end"      { ENDMEIOBLOCO } (* NAO TINHA*)
| "integer"  { INTEGER }
| "string"   { STRING }
| "real"     { REAL }
| "char"     { CHAR } 

| inteiro as num { let numero = int_of_string num in 
                    LITINT numero  } 
| flutuante as num {let numero = float_of_string num in 
                    LITFLOAT numero}
| "if"       { IF }
| "then"     { THEN }
| "else"     { ELSE }
| "case"     { CASE }
| "of"       { OF }
| "do"       { DO }
| "while"    { WHILE }
| "for"      { FOR }
| "to"       { TO }
| "write"    { WRITE }
| "writeln"  { WRITELN }
| "read"     { READ }
| "readln"   { READLN }

| identificador as id { ID id }
| '\'' |'"'        { let buffer = Buffer.create 1 in
                      let str = leia_string buffer lexbuf in
                       let tam = Buffer.length buffer in 
                         if(tam>1) then LITSTRING str   (*GAMBIARRA!!!*)
                         else LITCHAR str}
| _ as c  { failwith (caracter_erro lexbuf c) }
| eof        { EOF }
and comentario_bloco n b = parse
   "*)"   { if (b>n) then failwith (msg_erro lexbuf "Comentario")
               else comentario_bloco (n-1) b lexbuf }
|  "}"    { if (n>b) then failwith (msg_erro lexbuf "Comentario")
               else comentario_bloco n (b-1) lexbuf }
|  "(*"   { if (n==0 && b==0) then comentario_bloco (n+1) b lexbuf
                else comentario_bloco (n+1) b lexbuf }
|  "{"    { if (n==0 && b==0) then comentario_bloco n (b+1) lexbuf
                 else comentario_bloco n (b+1) lexbuf}
| _       { if (n==0 && b==0) then token lexbuf
              else comentario_bloco n b lexbuf }
 
| eof     { if(n>0 || b>0) then failwith (msg_erro lexbuf "Comentario") 
            else failwith  (msg_erro lexbuf "Comentario")}

and leia_string buffer = parse
   '\''    { Buffer.contents buffer}
| '\"'      {Buffer.contents buffer}
| "\\t"   { Buffer.add_char buffer '\t'; leia_string buffer lexbuf }
| '\''  { Buffer.add_char buffer '\''; leia_string buffer lexbuf }

| _ as c    {Buffer.add_char buffer c; leia_string buffer lexbuf}

(**| _ as c    { Buffer.add_char buffer c; leia_string buffer lexbuf }**)
| eof     { failwith (msg_erro lexbuf "String ")}
