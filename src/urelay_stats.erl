-module(urelay_stats).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, log/2, dump/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(stats, { windows, size }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public APU
%

start_link() ->
	gen_server:start_link({ local,?MODULE}, ?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

log(Stat,Value) ->
	gen_server:call(?MODULE, { log, Stat, Value }).

dump() ->
	gen_server:call(?MODULE, dump).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private APU
%

init([]) ->
	{ ok, #stats{ windows = [], size = 10 }}.

handle_call(stop,_From,Stats) ->
	{ stop, stopped, Stats };

handle_call(dump, _From, Stats = #stats{ windows = Windows }) ->
	lists:map(fun ({K,V}) -> io:format("~p = ~p~n", [ K, V]) end,
		Windows),
	{ reply, ok, Stats };

handle_call({ log, Stat, Value }, _From, Stats = #stats{ windows = Windows, size = Size }) ->
	case proplists:lookup(Stat,Windows) of 
		{ Stat, Values } -> [ _H | T ] = Values;
		none ->  T = [ 0 || _X <- lists:seq(2,Size) ]
	end,	
	{ reply, ok, Stats#stats{ windows = [ { Stat, T ++ [Value] } | proplists:delete(Stat,Windows) ]}};

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
