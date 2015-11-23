-module(convert07).
-export([main/0]).

main() ->
	{
		integer_to_list(0, 16),
		integer_to_list(10, 16),
		integer_to_list(12345, 16),
		integer_to_list(94826, 16),
		integer_to_list(-1251, 16),
		integer_to_list(-1251319816843165413546416346816236234623634734634263474573623534735462, 36)
	}.
