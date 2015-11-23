-module(bin28).
-export([main/0]).

main() ->
	Bin = <<10,20,30,40,50,60,70,80,90,100,110,120>>,
	<<Bin:27/bitstring>>.
