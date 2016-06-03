-module(urelay_ui_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/0, stop/0, update_timer/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(urelay_ui_server, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	spawn(?MODULE,update_timer,[]),
	{ ok, #urelay_ui_server{}}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	io:format("[urelay_ui_server] unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast(update,State) ->
	urelay_stats:dump(),	
	spawn(?MODULE,update_timer,[]),
	{ noreply, State };

handle_cast(Message,State) ->
	io:format("[urelay_ui_server] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	io:format("[urelay_ui_server] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.


update_timer() ->
	receive
	after 1000 ->
		gen_server:cast(?MODULE,update)
	end.
	
