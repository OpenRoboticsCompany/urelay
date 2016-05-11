-module(urelay_ui).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/1, stop/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(ui, { socket, port }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Port) ->
	gen_server:start_link({ local, ui }, ?MODULE, #ui{ port = Port }, []).

stop() ->
	gen_server:call(ui,stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init( UI = #ui{ port = Port }) ->
	{ ok, Socket } = gen_tcp:listen(Port, [ binary, { active, true }]),
	{ ok, UI#ui{ socket = Socket }}.
		
handle_call(stop,_From,UI) ->
	{ stop, stopped, UI };

handle_call(Message,_From,UI) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ reply, ok, UI }.

handle_cast(Message,UI) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, UI }.

handle_info({ tcp, _Client, Message }, UI ) ->
	io:format("~p~n", Message),
	{ noreply, UI };

handle_info(Message,UI) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, UI }.

terminate(Reason, #ui{ socket = Socket }) ->
	io:format("Stopping due to ~p~n", [ Reason ]),
	gen_tcp:close(Socket),
	ok.

code_change(_Old,UI,_Extra) ->
	{ ok, UI }.	
