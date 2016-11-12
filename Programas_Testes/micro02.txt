program "micro02"

var 
num1, num2 : integer;

begin
	writeln("Digite o primeiro numero: ");
	readln(num1);
	writeln("Digite o segundo numero: ");
	readln(num2);
	if(num1 > num2) then
	begin
		writeln("O primeiro numero ");
		write(num1);
		write("eh maior que o segundo ");
		write(num2);
	end;
	else
	begin
	    writeln("O segundo numero ");
		write(num2);
		write("eh maior que o primeiro ");
		write(num1);
	end;
end.
