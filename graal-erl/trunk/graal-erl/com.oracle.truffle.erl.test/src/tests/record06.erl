-module(record06).
-export([main/0]).
-record(person, {name, age, sex}).

is_teen(#person{age=Age}) when 10=<Age, Age<20 ->
	true;
is_teen(_) ->
	false.

count(List, Fun) ->
	count(0, List, Fun).

count(Count, [Head | Tail], Fun) ->
	case Fun(Head) of
		true  -> NewCount = Count + 1;
		false -> NewCount = Count
	end,
	count(NewCount, Tail, Fun);
count(Count, [], _Fun) ->
	Count.

main() ->
	Persons =
	[
		#person{name="Joe", age=18, sex=male},
		#person{name="John", age=30, sex=male},
		#person{name="Kate", age=26, sex=female},
		#person{name="Jane", age=19, sex=female}
	],
	count(Persons, fun is_teen/1).
