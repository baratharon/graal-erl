-module(bin52).
-export([main/0]).

main() ->
	Bin0 = <<0>>,                    %% 1
	Bin1 = <<Bin0/binary,1,2,3>>,    %% 2
	Bin2 = <<Bin1/binary,4,5,6>>,    %% 3
	Bin3 = <<Bin2/binary,7,8,9>>,    %% 4
	Bin4 = <<Bin1/binary,17>>,       %% 5 !!!
	{Bin4,Bin3}.                     %% 6
