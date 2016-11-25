all:
	./script.sh #ISTO ERA PRA TRAZER
	ocamlbuild -use-menhir lexicoTest.byte
	ocamlbuild -use-menhir sintaticoTest.byte
	ocamlbuild -use-menhir ast.byte
	ocaml .ocamlinit

#other:
#	cp ../Analisador_Lexico/lexico.mll .
#	mv lexico.mll auxiliar.mll
#	sed "s/(\*Demais open\*)/open Sintatico/g" auxiliar.mll > lexico.mll
#	rm auxiliar.mll
