program "micro05"

var
nome, sexo : char;
x,h,m : integer;

begin

for x := 1 to 5 do
begin
	writeln("Digite o nome: ");
	readln(nome);

	readln(sexo);
	case sexo of
		'H' : h:= h+1 ;
		'M' : m:= m+1 ;
	end;
end;

write("Foram inseridos: ");
write(h);
writeln(" homens");
write("Foram inseridos: ");
write(m);
write(" mulheres");

end.