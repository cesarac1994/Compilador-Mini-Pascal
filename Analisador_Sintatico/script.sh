#!/bin/bash

###################################################################
# TENTATIVA DE TRAZER O QUE MODIFICARA NO LEXICO DIRETO PRA CÁ... #
###################################################################

cp  ../Analisador_Lexico/lexico.mll .
mv lexico.mll auxiliar.mll
sed "s/(\*Demais open\*)/open Sintatico/g" auxiliar.mll > lexico.mll
rm auxiliar.mll




#S_ORIGINAL = ../Analisador_Lexico/lexico.mll"
#S_ARQUIVO = lexico.mll"
#ARQUIVOII = _build/lexico.mll
#MODIFICAORIGIN = $(stat --print=%y $ORIGINAL)
#MODIFICAARQ = $(stat --print=%y $ARQUIVO)

#if  ls $ARQUIVO 
#then
#	if $MODIFICAORIGIN != $MODIFICAARQ 
#	then
#   		rm $ARQUIVO
#	fi	
#fi
#
#if ls $ARQUIVOII 
#then
#	rm $ARQUIVOII
#fi

