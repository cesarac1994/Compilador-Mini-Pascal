#load "lexico.cmo";;

let rec tokens lexbuf =
  let tok = Lexico.token lexbuf in
  match tok with
  | Lexico.EOF -> [Lexico.EOF]
  | _ -> tok :: tokens lexbuf
;;

let lexico str =
  let lexbuf = Lexing.from_string str in
  tokens lexbuf
;;

let lex arq =
  let ic = open_in arq in
  let lexbuf = Lexing.from_channel ic in
  let toks = tokens lexbuf in
  let _ = close_in ic in
  toks
;;

print_string "\n\n";;
print_string "Para ler uma STRING e gerar os tokens, chame 'lexico STRING_ENTRE_ASPAS'";;
print_string "Para ler um ARQUIVO e gerar os tokens, chame 'lex ARQUIVO_ENTRE_ASPAS"



