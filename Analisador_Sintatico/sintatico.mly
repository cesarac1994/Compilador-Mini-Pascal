
%{
open Ast
%}

%token PROGRAMA
(* %token USES *)
%token VAR
%token INTEGER
%token CHAR
%token REAL
%token BEGIN
%token ENDPROGRAMA
%token ENDBLOCO
%token ENDMEIOBLOCO
%token PONTOVIRGULA
%token VIRGULA
%token APAR
%token FPAR
%token ATRIB
%token DOISPONTOS
%token NAO 
%token AND 
%token OR
%token XOR
%token MAIOR
%token MENOR
%token MAIORIGUAL
%token MENORIGUAL
%token IGUAL
%token DIFERENTE
%token SOMA
%token SUBTRACAO
%token DIVISAO
%token MULTIPLICACAO
%token RESTO
%token IF
%token CASE
%token OF
%token THEN
%token ELSE
%token WHILE
%token DO
%token FOR
%token TO 
%token DOWNTO
%token WRITE
%token WRITELN
%token READ
%token READLN
%token STRING
%token <int> LITINT
%token <string> ID
%token <string> LITSTRING
%token <string> LITCHAR
%token EOF

%left IGUAL DIFERENTE MAIORIGUAL MENORIGUAL MAIOR MENOR
%left SOMA SUBTRACAO
%left DIVISAO MULTIPLICACAO AND RESTO
%left NAO
      
       
%start <Ast.programa> programa

%%
  
programa: PROGRAMA
          VAR
            ds = declaracao*
          BEGIN
            cs = comando*
          ENDPROGRAMA
          EOF { Programa (List.flatten ds, cs) }




declaracao: ids = separated_nonempty_list(VIRGULA, ID) DOISPONTOS t = tipo_simples PONTOVIRGULA {
                   List.map (fun id -> DecVar (id,t)) ids
          }
 
tipo_simples: INTEGER { TipoInt }
            | STRING  { TipoString } 
            | CHAR    { TipoChar }
            | REAL    { TipoReal }

(*-----------------------------CONTINUAR DAKI--------------------------*)
(*--------------------------------ALTERAR O AST TAMBÉM------------------*)

comando: c=comando_atribuicao { c }
       | c=comando_condicao   { c }
       | c=comando_laco       { c } (*NÃO TINHA*)
       | c=comando_entrada    { c }
       | c=comando_saida      { c }

(*NÃO TINHA*)
bloco: bs = comando          { Comando bs }      (* simples *) /*parece redundante, mas não é!!!*/ 
     | BEGIN bmdu = comando+ { Comandos bmdu }   (* mais de um *) 

(*NÃO TINHA*)
blocoOuIfAninhado:  bi = bloco {Bloco bi}
                  | se = comando_ifThen {AndIf se}

(*NÃO TINHA*)
if_cabecalho_bloco: IF logica=expressao THEN logo=blocoOuIfAninhado { Se(logica,logo) }

(*NÃO TINHA*)
comando_ifThen: ci = if_cabecalho_bloco
                 senao=option(ENDMEIOBLOCO ELSE entao=blocoOuIfAninhado {Senao entao}) (*com else*)
                 final=option(ENDBLOCO) (*sem else, ou com else com mais de 1 comando*)
                { If_ (entao, senao) }

(*NÃO TINHA*)
caseOf_cabecalho: CASE algo=expressao OF {Case algo}

(*NÃO TINHA*)
caseOF_OpcCom: opc=opcao DOISPONTOS 
               com=blocoOuIfAninhado 
               ptvirg=option(PONTOVIRGULA) (*se estiver apenas 1 comando*)
               endptvirg=option(ENDBLOCO) (*se estiver com mais de 1 comando, o begin não fecha sozinho!!!*)
               {OpcaoComandoCaseOf(opc,com)}

(*NÃO TINHA*)
caseOF_OpcaoComandoBloco: blocoOpcaoComando = caseOF_OpcCom+ {BlockOpcaoComandoCaseOf blocoOpcaoComando}


(*NÃO TINHA*)
comando_caseOf: cabecalho=caseOf_cabecalho
                opc_comando=caseOF_OpcaoComandoBloco
                senao=option(ELSE portanto=blocoOuIfAninhado {Senao portanto}) (*com else*)
                ENDBLOCO {caseOF(cabecalho, opc_comando, senao)}


comando_condicao: ci = comando_ifThen { ci } 
                | cc = comando_caseOf { cc }
                

comando_atribuicao: v=variavel ATRIB e=expressao PONTOVIRGULA {
      CmdAtrib (v,e)
}

variavel: x=ID  { VarSimples x }
opcao: o = LITINT { Opcao o }
      |o = LITCHAR   { Opcao o }

expressao:
         | v=variavel { ExpVar v    }
         | i=LITINT      { ExpInt i    }
         | s=LITSTRING   { ExpString s }
         | APAR e=expressao FPAR { e }
         | e1=expressao op=oper e2=expressao { ExpOp (op, e1, e2) }
         | NAO APAR e=expressao FPAR { Nao e }


comando_entrada: READ xs=separated_nonempty_list(VIRGULA, variavel) PONTOVIRGULA {CmdRead xs }
                |READLN xs=separated_nonempty_list(VIRGULA, variavel) PONTOVIRGULA {CmdReadln xs }


comando_saida: WRITE xs=separated_nonempty_list(VIRGULA, variavel) PONTOVIRGULA {CmdWrite xs}
              |WRITELN xs=separated_nonempty_list(VIRGULA, variavel) PONTOVIRGULA {CmdWriteln xs}

(*-----------------------------CONTINUAR DAKI--------------------------*)
(*--------------------------------ALTERAR O AST TAMBÉM------------------*)
(*NÃO TINHA*)
comando_laco: cw = comando_whileDo { cw }
            | cf = comando_forDo   { cf }

(*NÃO TINHA*)
comando_whileDo: WHILE enquato=expressao DO 
               oquefazer=blocoOuIfAninhado 
               ptvirg=option(PONTOVIRGULA) (*se estiver apenas 1 comando*)
               endptvirg=option(ENDBLOCO) (*se estiver com mais de 1 comando, o begin não fecha sozinho!!!*)
               {CmdWhile(enquato, oquefazer)}

(*NÃO TINHA*)
for_cabecalho: FOR varLimitInf=variavel ATRIB limitInf=LITINT TO limitSup=LITINT DO { Intervalo(limitInf,limitSup) } 
            | FOR varLimitInf=variavel ATRIB limitSupe=LITINT DOWNTO limitInfe=LITINT DO { Intervalo(limitSupe,limitInfe) }
comando_forDo: itervalo=for_cabecalho
              oquefazer=blocoOuIfAninhado 
              ptvirg=option(PONTOVIRGULA) (*se estiver apenas 1 comando*)
              endptvirg=option(ENDBLOCO) (*se estiver com mais de 1 comando, o begin não fecha sozinho!!!*)
              {CmdFor(intervalo,oquefazer)}

/* (* **********CONCLUIR O LAÇO E COMEÇAR O funçoes******** *) */
         
%inline oper: SOMA           { Soma  }
            | SUBTRACAO      { Subtracao }
            | MULTIPLICACAO  { Multiplicacao }
            | DIVISAO        { Divisao }
            | RESTO          { Resto }
            | MENOR          { Menor }
            | MAIOR          { Maior }
            | MENORIGUAL     { MenorIgual }
            | MAIORIGUAL     { MaiorIgual }
            | IGUAL          { Igual }
            | DIFERENTE      { Diferente }
            | AND            { And }
            | OR             { Or }
            | XOR            { Xor }

(*
%inline funcaoSaida:  WRITE { Write }
                      | WRITELN { Writeln }
                     
%inline funcaoEntrada:  READ { Read }
                      | READLN { Readln }

*)
