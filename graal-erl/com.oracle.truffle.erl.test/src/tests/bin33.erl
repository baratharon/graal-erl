-module(bin33).
-export([main/0]).

main() ->
	Bin = <<10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160>>,
	Num = 111,
	{
		begin <<N1:Num,_/bitstring>> = Bin, N1 end,
		begin <<N2:Num/little,_/bitstring>> = Bin, N2 end,
		begin <<N3:Num/signed,_/bitstring>> = Bin, N3 end,
		begin <<N4:Num/little-signed,_/bitstring>> = Bin, N4 end
	}.
