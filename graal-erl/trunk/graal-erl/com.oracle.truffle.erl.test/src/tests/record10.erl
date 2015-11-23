-module(record10).
-export([main/0]).
-record(person, {name="?", age=infinity, sex=alien}).

names_of_not_females(#person{sex=female}) ->
	false;
names_of_not_females(#person{name=Name}) ->
	{true, Name}.

names_of_females(#person{sex=female, name=Name}) ->
	{true, Name};
names_of_females(_) ->
	false.

main() ->
	Persons =
	[
		#person{name="Joe", age=18, sex=male},
		#person{name="John", age=30, sex=male},
		#person{name="Kate", age=26, sex=female},
		#person{name="Jane", age=19, sex=female},
		#person{name="alien", age=1000},
		#person{name="emperor"}
	],
	{
		lists:filtermap(fun names_of_not_females/1, Persons),
		lists:filtermap(fun names_of_females/1, Persons)
	}.
