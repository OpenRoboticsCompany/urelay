-module(urelay_stats).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, log/2, dump/0, allot/2, clear/1, sum/1, average/1, minimum/1, maximum/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(stats, { windows, sizes }).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private APU
%

init([]) ->
	{ ok, #stats{ windows = [], sizes = [] }}.

handle_call(stop,_From,Stats) ->
	{ stop, stopped, Stats };

handle_call(dump, _From, Stats = #stats{ windows = Windows }) ->
	lists:map(fun ({K,V}) -> io:format("~p = ~p~n", [ K, V]) end,
		Windows),
	{ reply, ok, Stats };

handle_call({ allot, Stat, Size}, _From, Stats) ->
	{ reply, ok, allocate(Stats,Stat,Size) };

handle_call({ log, Stat, Value }, _From, Stats) ->
	{ reply, ok, rotate(Stats,Stat,Value) };

handle_call({ clear, Stat }, _From, Stats) ->
	{ reply, ok, reset(Stats,Stat) };

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

handle_call(Message,_From,Stats) ->
	io:format("[stats] unknown message ~p~n", [ Message ]),
	{ reply, ok, Stats }.

handle_cast(Message,Stats) ->
	io:format("[stats] unknown message ~p~n", [ Message ]),
	{ noreply, Stats }.

handle_info(Message,Stats) ->
	io:format("[stats] unknown message ~p~n", [ Message ]),
	{ noreply, Stats }.

code_change(_Old,_Extra,Stats) ->
	{ ok, Stats }.

terminate(_Reason,_Stats) ->
	ok.

populate(Size) ->
	[ 0 || _X <- lists:seq(1,Size) ].

allocate(Stats = #stats{ windows = Windows, sizes = Sizes },Stat,Size) ->
	Stats#stats{
		windows = [ { Stat, populate(Size) } | proplists:delete(Stat,Windows) ],
		sizes = [ { Stat, Size } | proplists:delete(Stat,Sizes) ]
	}.

rotate(Stats = #stats{ windows = Windows }, Stat, Value) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, Values } -> [ _H | T ] = Values;
		none -> T = []	%% assume we are only tracking a single value
	end,	
	Stats#stats{ windows = [ { Stat, T ++ [Value] } | proplists:delete(Stat,Windows) ]}.

reset(Stats = #stats{ windows = Windows, sizes = Sizes }, Stat) ->
	case proplists:lookup(Stat,Sizes) of
		{ Stat, Size } -> Window = populate(Size);
		none -> Window = [0]
	end,
	Stats#stats{ windows =  [ { Stat, Window } | proplists:delete(Stat,Windows) ]}.



