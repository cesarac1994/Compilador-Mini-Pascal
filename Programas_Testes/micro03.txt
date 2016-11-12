program "micro03"

var
numero : integer;

begin
	writeln("Digite um numero: ");
	readln(numero);
	if(numero >= 100) then
	begin
		if(numero <= 200) then
		begin
			writeln("O numero esta no intervalo entre 100 e 200");
		end;
		else begin
			writeln("O numero nao esta no intervalo entre 100 e 200");
		end;
	end;
	else begin
		writeln("O numero nao esta no intervalo entre 100 e 200");
	end;

end.