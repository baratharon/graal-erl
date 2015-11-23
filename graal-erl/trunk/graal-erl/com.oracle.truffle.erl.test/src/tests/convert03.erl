-module(convert03).
-export([main/0]).

main() ->
	{
		list_to_integer("0", 2),
		list_to_integer("10", 2),
		list_to_integer("12345", 7),
		list_to_integer("94826", 11),
		list_to_integer("-1251", 31),
		list_to_integer("-1251319816843165413546416346816236234623634734634263474573623534735462", 20)
	}.
