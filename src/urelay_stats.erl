-module(urelay_stats).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, log/2, dump/0, allot/2, clear/1, sum/1, average/1, minimum/1, maximum/1, increment/1, decrement/1, add/2, sub/2 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2 ]).

-include("urelay.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public APU
%

start_link() ->
	gen_server:start_link({ local,?MODULE}, ?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

allot(Stat,Size) ->
	gen_server:call(?MODULE, { allot, Stat, Size }).

log(Stat,Value) ->
	gen_server:call(?MODULE, { log, Stat, Value }).

dump() ->
	gen_server:call(?MODULE, dump).

clear(Stat) ->
	gen_server:call(?MODULE, { clear, Stat }).

sum(Stat) ->
	gen_server:call(?MODULE, { sum, Stat }).

average(Stat) ->
	gen_server:call(?MODULE, { avg, Stat }).

minimum(Stat) ->
	gen_server:call(?MODULE, { min, Stat }).

maximum(Stat) ->
	gen_server:call(?MODULE, { max, Stat }).

increment(Stat) ->
	gen_server:call(?MODULE, { inc, Stat }).

decrement(Stat) ->
	gen_server:call(?MODULE, { dec, Stat }).

add(Stat,Value) ->
	gen_server:call(?MODULE, { add, Stat, Value }).

sub(Stat,Value) ->
	gen_server:call(?MODULE, { sub, Stat, Value }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private APU
%

init([]) ->
	{ ok, #stats{ windows = [], sizes = [], counter = 0, per_second = [], per_minute = [], per_hour = [] }}.

handle_call(stop,_From,Stats) ->
	{ stop, stopped, Stats };

handle_call(dump, _From, Stats = #stats{ windows = Windows, per_second = PerSecond, per_minute = PerMinute, per_hour = PerHour }) ->
	lists:map(fun ({K,V}) -> urelay_log:log(?MODULE,"~p = ~p~n", [ K, V]) end, Windows),
	lists:map(fun ({K,V}) -> urelay_log:log(?MODULE,"~p per second = ~p~n", [ K, V]) end, PerSecond),
	lists:map(fun ({K,V}) -> urelay_log:log(?MODULE,"~p per minute = ~p~n", [ K, V]) end, PerMinute),
	lists:map(fun ({K,V}) -> urelay_log:log(?MODULE,"~p per hour = ~p~n", [ K, V]) end, PerHour),
	{ reply, ok, Stats };

handle_call({ allot, Stat, Size}, _From, Stats = #stats{ windows = Windows, sizes = Sizes }) ->
	{ W, S } = allocate(Windows,Sizes,Stat,Size),
	{ reply, ok, Stats#stats{ windows = W, sizes = S } };

handle_call({ log, Stat, Value }, _From, Stats = #stats{ windows = Windows }) ->
	{ reply, ok, Stats#stats{ windows = rotate(Windows,Stat,Value) } };

handle_call({ clear, Stat }, _From, Stats = #stats{ sizes = Sizes, windows = Windows}) ->
	{ reply, ok, Stats#stats{ windows =  reset(Windows,Sizes,Stat) }};

handle_call({ sum, Stat }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, Values } -> Sum = lists:foldl(fun(A,B) -> A+B end, 0, Values);
		none -> Sum = 0
	end,
	{ reply, Sum, Stats };

handle_call({ avg, Stat }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, Values } -> Avg = lists:foldl(fun(A,B) -> A+B end, 0, Values)/length(Values);
		none -> Avg = 0
	end,
	{ reply, Avg, Stats };

handle_call({ min, Stat }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, [H|T] } -> Min = lists:foldl(fun(A,B) -> min(A,B) end, H, T);
		none -> Min = 0
	end,
	{ reply, Min, Stats };

handle_call({ max, Stat }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
			{ Stat, [H|T] } -> Max = lists:foldl(fun(A,B) -> max(A,B) end, H, T);
		none -> Max = 0
	end,
	{ reply, Max, Stats };

handle_call({ inc, Stat }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, [ H | _T ] } -> V = H + 1;
		none -> V = 1
	end,
	{ reply, V, Stats#stats{ windows = rotate(Windows,Stat,V) } };

handle_call({ dec, Stat }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, [ H | _T ] } -> V = max(H - 1,0);
		none -> V = 0		% we can't go below 0
	end,
	{ reply, V, Stats#stats{ windows = rotate(Windows,Stat,V) } };

handle_call({ add, Stat, Value }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, [ H | _T ] } -> V = H + Value ;
		none -> V = Value
	end,
	{ reply, V, Stats#stats{ windows = rotate(Windows,Stat,V) } };

handle_call({ sub, Stat, Value }, _From, Stats = #stats{ windows = Windows }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, [ H | _T ] } -> V = max(H - Value,0);
		none -> V = 0	
	end,
	{ reply, V, Stats#stats{ windows = rotate(Windows,Stat,V) } };

handle_call(Message,_From,Stats) ->
	urelay_log:log(?MODULE,"[stats] unknown message ~p~n", [ Message ]),
	{ reply, ok, Stats }.

handle_cast(timer, Stats = #stats{ counter = Counter }) ->
	Stats2 = roll_per_second_stats(Stats),
	Stats3 = roll_per_minute_stats(Stats2),
	Stats4 = roll_per_hour_stats(Stats3),
	{ ok, Timer } = timer:apply_after(1000, gen_server, cast, [ ?MODULE, timer ]),	% after a second roll the stats again
	{ noreply, Stats4#stats{ timer = Timer, counter = Counter + 1 } };
	
handle_cast(Message,Stats) ->
	urelay_log:log(?MODULE,"handle_cast unknown message ~p~n", [ Message ]),
	{ noreply, Stats }.

handle_info(Message,Stats) ->
	urelay_log:log(?MODULE,"handle_info unknown message ~p~n", [ Message ]),
	{ noreply, Stats }.

code_change(_Old,_Extra,Stats) ->
	{ ok, Stats }.

terminate(_Reason,_Stats) ->
	ok.

populate(Size) ->
	[ 0 || _X <- lists:seq(1,Size) ].

allocate(Windows,Sizes,Stat,Size) ->
	{ [ { Stat, populate(Size) } | proplists:delete(Stat,Windows) ],
	  [ { Stat, Size } | proplists:delete(Stat,Sizes) ] }.

rotate(Windows,Stat,Value) ->
	case proplists:lookup(Stat,Windows) of
		{ Stat, Values } -> [ _H | T ] = Values;
		none -> T = [] %% assume we are only tracking a single value
	end,
	[ { Stat, T ++ [ Value ]} | proplists:delete(Stat,Windows) ].

rotate(Windows,Stat,Value,Size) ->
	case proplists:lookup(Stat,Windows) of
		{ Stat, Values } -> [ _H | T ] = Values;
		none -> T = populate(Size - 1) %% assume we are only tracking a single value
	end,
	[ { Stat, T ++ [ Value ]} | proplists:delete(Stat,Windows) ].


reset( Windows, Sizes, Stat ) ->
	case proplists:lookup(Stat,Sizes) of
		{ Stat, Size } -> Window = populate(Size);
		none -> Window = [0]
	end,
	[ { Stat, Window } | proplists:delete(Stat,Windows) ].


roll_per_second_stats(Stats = #stats{ windows = Windows, per_second = PerSecond }) ->
	PS2 = lists:foldl(fun({K,V},PS) ->
		Sum = lists:foldl(fun(A,B) -> A+B end, 0, V),
		rotate(PS,K,Sum,60)
	end,  PerSecond, Windows),
	Stats#stats{ per_second = PS2 }.

roll_per_minute_stats(Stats = #stats{ counter = Counter, per_second = PerSecond, per_minute = PerMinute }) when Counter rem 60 =:= 0 ->
	PM2 = lists:foldl(fun({K,V},PM) ->
		Sum = lists:foldl(fun(A,B) -> A+B end, 0, V),
		rotate(PM,K,Sum,60)
	end, PerMinute, PerSecond),
	Stats#stats{ per_minute = PM2 };
roll_per_minute_stats(Stats) ->
	Stats.

roll_per_hour_stats(Stats = #stats{ counter = Counter, per_minute = PerMinute, per_hour = PerHour }) when Counter rem 3600 =:= 0 ->
	PH2 = lists:foldl(fun({K,V},PH) ->
		Sum = lists:foldl(fun(A,B) -> A+B end, 0, V),
		rotate(PH,K,Sum,24)
	end, PerHour, PerMinute),
	Stats#stats{ per_hour = PH2 };
roll_per_hour_stats(Stats) ->
	Stats.


