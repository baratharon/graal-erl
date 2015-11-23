-module(record15).
-export([main/0]).
-record(rec, {field1=1*100+2*10+3*1, field2=(((1*10+2)*10+3)*10+4), field3={default,value}}).

main() ->
	Rec = #rec{},
	Rec.
