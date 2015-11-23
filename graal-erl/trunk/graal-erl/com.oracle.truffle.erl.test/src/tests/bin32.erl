-module(bin32).
-export([main/0]).

main() ->
	Bin = <<250,66,77,87,90,100,110,120,130,140,150,160>>,
	Num = 27,
	{
		begin <<N1:Num,_/bitstring>> = Bin, N1 end,
		begin <<N2:Num/little,_/bitstring>> = Bin, N2 end,
		begin <<N3:Num/signed,_/bitstring>> = Bin, N3 end,
		begin <<N4:Num/little-signed,_/bitstring>> = Bin, N4 end
	}.
