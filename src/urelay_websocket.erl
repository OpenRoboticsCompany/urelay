-module(urelay_websocket).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ relay/2, start_link/2 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2,
	init/1, terminate/2 ]).

-include("urelay.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

start_link(WebSocket,UUID) ->
	urelay_log:log(?MODULE, "Start Link ~p ~p~n", [ WebSocket, UUID ]),
	gen_server:start_link({ local, UUID }, ?MODULE, [ WebSocket ], []).

relay(WebSocket,connected) ->
	UUID = websocket:uuid(WebSocket),
	urelay_log:log(?MODULE, "Connected websocket ~p~n", [ UUID ]),
	supervisor:start_child(urelay_websocket_supervisor, #{
		id => UUID,
		start => { urelay_websocket, start_link, [ WebSocket, UUID ] },
		shutdown => brutal_kill,
		restart => temporary,
		worker => worker });

relay(ID,closed) ->
	gen_server:call(ID, closed );

relay(WebSocket,Data) ->
	ID = websocket:uuid(WebSocket),
	JSON = json:decode(Data),
	UJSON = ujson:encode(JSON),
	gen_server:call(ID, { relay, WebSocket, UJSON }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

init([WebSocket]) ->
	urelay_log:log(?MODULE,"initializing for ~p~n", [ WebSocket ]),
	{ ok, Socket } = gen_udp:open(0, [ binary, { active, true }, { recbuf, 65536 }, { reuseaddr, true }]),	 %% 0 means OS supply
	urelay_log:log(?MODULE,"opened socket ~p~n", [ Socket ]),
	{ ok, Port } = inet:port(Socket),
	urelay_log:log(?MODULE,"socket port ~p~n", [ Port ]),
	[ $/ | Path ] = websocket:path(WebSocket),
	
	Room = list_to_atom(Path),
	UUID = websocket:uuid(WebSocket),
	urelay_log:log(?MODULE,"connecting to ~p for id ~p ~n",  [ Room, UUID ]),
	urelay_room:join(Room,{127,0,0,1},Port,UUID, urelay_room, nofilter, []), 
	urelay_stats:increment(websockets),
	urelay_stats:increment(websocket_connections),
	{ ok, #relay{ websocket = WebSocket, socket = Socket, port = Port, room =  Room, id = UUID }}.

handle_call(closed, _From, Relay) ->
	{ stop, normal, Relay };

handle_call({ relay, WebSocket, Data }, _From, Relay = #relay{ websocket = WebSocket, room = Room, id = Id }) ->
	urelay_room:broadcast(Room,Id,Data),
	urelay_stats:add(websocket_bytes_in,size(Data)),
	{ reply, ok, Relay };
	
handle_call(Message, _From, Relay ) ->
	urelay_log:log(?MODULE,"unknown message ~p~n", [ Message ]),
	{ reply, ok, Relay }.

handle_cast(Message, Relay ) ->
	urelay_log:log(?MODULE,"unknown message ~p~n", [ Message ]),
	{ noreply, Relay }.

handle_info({ udp, _Client, _IPAddr, _Port, Packet }, Relay = #relay{ websocket = WebSocket }) ->
	{ UJSON, _Rem }= ujson:decode(Packet),
	JSON = json:encode(UJSON),
	urelay_stats:add(websocket_bytes_out,size(JSON)),
	websocket:send(WebSocket,JSON),
	{ noreply, Relay };

handle_info(Message,Relay) ->
	urelay_log:log(?MODULE,"unknown message ~p~n", [ Message ]),
	{ noreply, Relay }.

terminate(Reason, #relay{ room = Room, id = Id }) ->
	urelay_room:leave(Room,Id),
	urelay_stats:decrement(websockets),
	urelay_stats:increment(websocket_disconnects),
	urelay_log:log(?MODULE,"shutting down because ~p~n", [ Reason ]),
	ok.

code_change(_Old, Relay, _Extra) ->
	{ ok, Relay }.	

