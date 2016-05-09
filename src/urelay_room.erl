-module(urelay_room).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/2, 
	join/2, leave/2, ban/2, unban/2, peer/2,
	message/3, broadcast/2, 
	users/1, bans/1, peers/1,
	close/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(peer, { ipaddr, port }).
-record(user, { ipaddr, port, name }).
-record(room, { name, users, bans, peers, socket, port }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API

start_link(Room, Port) ->
	gen_server:start_link({ local, Room }, ?MODULE, [ Room, Port ], []).

join(Room,Id) ->
	gen_server:call(Room, { join, Id }).

leave(Room,Id) ->
	gen_server:call(Room, { leave, Id }).
	
ban(Room,Id) ->
	gen_server:call(Room, { ban, Id } ).

unban(Room,Id) ->
	gen_server:call(Room, { unban, Id } ).

peer(Room,Peer) ->
	gen_server:call(Room, { peer, Peer }).

message(Room,Id,Message) ->
	gen_server:call(Room, { message, Id, Message }).

broadcast(Room,Message) ->
	gen_server:call(Room, { broadcast, Message }).

users(Room) ->
	gen_server:call(Room, users).

bans(Room) ->
	gen_server:call(Room, bans).

peers(Room) ->
	gen_server:call(Room, peers).

close(Room) ->
	gen_server:call(Room, close).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([ Name, Port ]) ->
	io:format("Starting room ~p on port ~p~n", [ Name, Port ]),
	{ ok, Socket } = gen_udp:open(Port, [ binary, { active, true }]),
	{ ok, #room{ name = Name, users = [], bans = [], peers = [], 
		port = Port, socket = Socket }}.

handle_call( { join, Id }, _From, Room = #room{ users = Users, bans = Bans }) ->
	case lists:member( Id, Bans ) of
		true ->
			{ reply, { banned, Id }, Room };	
		false ->	
			{ reply, ok, Room#room{ users = [ Id | Users ] } }
	end;

handle_call( { leave, Id }, _From, Room = #room{ users = Users }) ->
	{ reply, ok, Room#room{ users = users:delete(Id,Users) }};

handle_call( { ban, Id }, _From, Room = #room{ users = Users, bans = Bans }) ->
	{ reply, ok, Room#room{ 
		users = lists:delete(Id,Users),
		bans = [ Id | Bans ] }};

handle_call( { unban, Id }, _From, Room = #room{ bans = Bans }) ->
	{ reply, ok, Room#room{ bans = lists:delete(Id,Bans) }};

handle_call( { peer, Peer = #peer{} }, _From, Room = #room{ peers = Peers }) ->
	{ reply, ok, Room#room{ peers = [ Peer | Peers ] }};

handle_call( { message, #user{ ipaddr = IPAddr, port = Port }, Message }, _From, Room = #room{ socket = Socket }) ->
	gen_udp:send(Socket,IPAddr,Port,Message),
	{ reply, ok, Room };

handle_call( { broadcast, Message }, _From, Room = #room{ 
	users = Users, peers = Peers, socket = Socket }) ->
	[ broadcast(Socket,U,Message) || U <- Users ],
	[ relay(Socket,P,Message) || P <- Peers ],
	{ reply, ok, Room };

handle_call( users, _From, Room = #room{ users = Users }) ->
	{ reply, { ok, Users }, Room };

handle_call( bans, _From, Room = #room{ bans = Bans }) ->
	{ reply, { ok, Bans }, Room };

handle_call( peers, _From, Room = #room{ peers = Peers }) ->
	{ reply, { ok, Peers }, Room };

handle_call( close, _From, Room = #room{ socket = Socket }) ->
	gen_udp:close(Socket),
	{ reply, { stop, ok }, Room };

handle_call( _Message, _From, Room ) ->
	{ reply, ok, Room }.

handle_cast( _Message, Room ) ->
	{ noreply, Room }.

handle_info({ udp, _Client, IPAddr, Port, Packet }, Room = #room{ users = Users, bans = Bans, peers = Peers, socket = Socket }) ->
	case member(IPAddr,Port,Bans) of
		true -> io:format("[Banned] ~p:~p ~p~n", [ IPAddr,Port,Packet]);
		false -> false
	end,
	case member(IPAddr,Port,Users) of
		true -> 
			io:format("[Relay] ~p:~p ~p~n", [ IPAddr,Port,Packet]),
			[ broadcast(Socket,U,Packet) || U <- except(IPAddr,Port,Users) ],
			[ relay(Socket,P,Packet) || P <- Peers];
		false -> false
	end,
	{ noreply, Room };

handle_info( Message, Room ) ->
	io:format("Got message ~p~n", [ Message ]),
	{ noreply, Room }.

terminate(Reason, #room{ name = Name } ) ->
	io:format("Shutting down ~p because ~p~n", [ Name, Reason ]),
	ok.

code_change( _Old, Room, _Extra) ->
	{ ok, Room }.	


broadcast(Socket, #user{ ipaddr = IPAddr, port = Port }, Message ) ->
	gen_udp:send(Socket,IPAddr,Port,Message).

relay(Socket, #peer{ ipaddr = IPAddr, port = Port }, Message ) ->
	gen_udp:send(Socket,IPAddr,Port,Message).

member(IPAddr,Port,Users) ->
	[] /= lists:filtermap( fun(#user{ ipaddr = I, port = P }) -> 
		(I =:= IPAddr) and (P =:= Port) end, Users).
	
except(IPAddr,Port,Users) ->
	lists:filtermap( fun(#user{ ipaddr = I, port = P }) ->
		(I /= IPAddr) or (P /= Port) end, Users ).
