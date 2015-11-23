-module(convert01).
-export([main/0]).

main() ->
	{
		list_to_integer("0"),
		list_to_integer("10"),
		list_to_integer("12345"),
		list_to_integer("94826"),
		list_to_integer("-1251"),
		list_to_integer("-1251319816843165413546416346816236234623634734634263474573623534735462")
	}.
