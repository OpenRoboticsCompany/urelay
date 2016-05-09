-module(urelay_websocket).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/3, close/0, relay/2 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2,
	init/1, terminate/2 ]).

-record(relay, { room, wssuper, wsport, socket, port, websockets }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

start_link(Room,WSPort,Port) ->
	gen_server:start_link({ local, ?MODULE}, ?MODULE, [ Room, WSPort, Port ], []).

close() ->
	gen_server:call(?MODULE, close).

relay(WebSocket,Data) ->
	io:format("Relay called ~p~n", [ Data ]),
	JSON = json:decode(Data),
	UJSON = ujson:encode(JSON),
	gen_server:call(?MODULE, { relay, WebSocket, UJSON }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods
%%

init([ Room, WSPort, Port ]) ->
	{ ok, Super } = urelay_websocket_supervisor:start_link(WSPort),
	{ ok, Socket } = gen_udp:open(Port, [ binary, { active, true }]),
	{ ok, #relay{
		room = Room,
		wssuper = Super, wsport = WSPort,
		socket = Socket, port = Port,
		websockets = sets:new() }}.

handle_call( close, _From, Relay ) ->
	{ stop, closed, Relay };

handle_call( { relay, WebSocket, Data }, _From, Relay = #relay{ socket = Socket , room = { RoomIP, RoomPort }, websockets = WebSockets }) ->
	io:format("Relay ~p to ~p:~p~n", [ Data, RoomIP, RoomPort ]),
	gen_udp:send( Socket, RoomIP, RoomPort, Data ),
	{ reply, ok, Relay#relay{ websockets = sets:add_element(WebSocket,WebSockets) }};
	

handle_call( Message, _From, Relay ) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ reply, ok, Relay }.

handle_cast( Message, Relay ) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, Relay }.

handle_info({ udp, _Client, IPAddr, Port, Packet }, Relay = #relay{ websockets = WebSockets }) ->
	io:format("Got message from ~p:~p", [ IPAddr, Port ]),
	{ UJSON, _Rem }= ujson:decode(Packet),
	JSON = json:encode(UJSON),
	[ websocket:send(W,JSON) || W <- sets:to_list(WebSockets) ],
	{ noreply, Relay };

handle_info(Message,Relay) ->
	io:format("Unknown message ~p~n", [ Message ]),
	{ noreply, Relay }.

terminate(Reason, #relay{ socket = Socket }) ->
	gen_udp:close(Socket),
	io:format("Shutting down because ~p~n", [ Reason ]),
	ok.

code_change(_Old, Relay, _Extra) ->
	{ ok, Relay }.	
