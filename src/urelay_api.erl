
-module(urelay_room).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/1, close/0, rooms/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(api, { socket, port, rooms, supervisor }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Port) ->
	gen_server:start_link( { local, api }, ?MODULE, [ Port ], []).

close() ->
	gen_server:call(api,stop).

rooms() ->
	gen_server:call(api,rooms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([ Port ]) ->
	


handle_call(stop,_From,API) ->
	{ stop, ok, API };

handle_call(rooms,_From,API #api{ rooms = Rooms }) ->
	{ reply, { ok, Rooms }, API }.

handle_call(Message, _From, API) ->
	io:format("Got message ~p~n", [ Message ]),
	{ reply, ok, API }.

handle_info({ udp, _Socket, IPAddr, Port, Packet }, API) ->
	Command = json:decode(Packet),
	dispatch(Command);

handle_info(Message,API) ->
	io:format("Got message ~p~n", [ Message ]).	

terminate(Reason,API = #{ socket = Socket }) ->
	io:format("Stopping ~p because ~p~n", [ API, Reason ]),
	gen_udp:close(Socket),
	ok.

code_change(_Old,_Vsn,API) ->
	{ ok, API }.

dispatch(
