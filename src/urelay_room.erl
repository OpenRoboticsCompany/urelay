-module(urelay_room).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/2, 
	join/7, leave/2, ban/2, unban/2, peer/2,
	message/3, broadcast/3, 
	users/1, bans/1, peers/1,
	close/1, nofilter/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-include("urelay.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API

start_link(Room, Port) ->
	gen_server:start_link({ local, Room }, ?MODULE, [ Room, Port ], []).

join(Room, IPAddr, Port, Id, Module, Function, Args) ->
	gen_server:call(Room, { join, #user{ ipaddr = IPAddr, port = Port, name = Id, filter_module = Module, filter_function = Function, filter_args = Args }}).

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

broadcast(Room,Id,Message) ->
	gen_server:call(Room, { broadcast, Id, Message }).

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
	urelay_log:log(?MODULE,"Starting room ~p on port ~p~n", [ Name, Port ]),
	{ ok, Socket } = gen_udp:open(Port, [ binary, { active, true }, { recbuf, 65536} ]),
	{ ok, #room{ name = Name, users = [], bans = [], peers = [], 
		port = Port, socket = Socket }}.

handle_call( { join, Id }, _From, Room = #room{ users = Users, bans = Bans }) ->
	urelay_stats:increment(room_joins),
	case lists:member( Id, Bans ) of
		true ->
			{ reply, { banned, Id }, Room };	
		false ->	
			{ reply, ok, Room#room{ users = [ Id | lists:delete(Id,Users) ] } }
	end;

handle_call( { leave, Id }, _From, Room = #room{ users = Users }) ->
	urelay_stats:increment(room_leaves),
	Rem = lists:filter(fun(#user{ name = Name }) -> 
		Name =/= Id end, Users), 
	{ reply, ok, Room#room{ users = Rem }};

handle_call( { ban, Id }, _From, Room = #room{ users = Users, bans = Bans }) ->
	urelay_stats:increment(room_bans),
	{ reply, ok, Room#room{ 
		users = lists:delete(Id,Users),
		bans = [ Id | Bans ] }};

handle_call( { unban, Id }, _From, Room = #room{ bans = Bans }) ->
	urelay_stats:increment(room_unbans),
	{ reply, ok, Room#room{ bans = lists:delete(Id,Bans) }};

handle_call( { peer, Peer = #peer{} }, _From, Room = #room{ peers = Peers }) ->
	urelay_stats:increment(room_peers),
	{ reply, ok, Room#room{ peers = [ Peer | Peers ] }};

handle_call( { message, #user{ ipaddr = IPAddr, port = Port }, Message }, _From, Room = #room{ socket = Socket }) ->
	urelay_log:log(?MODULE,"sending to ~p:~p ~p~n", [ IPAddr, Port, Message ]),
	gen_udp:send(Socket,IPAddr,Port,Message),
	urelay_stats:add(room_message_bytes_out, size(Message)),
	{ reply, ok, Room };

handle_call( { broadcast, Id, Message }, _From, Room = #room{ 
	users = Users, peers = Peers, socket = Socket }) ->
	Recipients = lists:filter( fun(#user{ name = Name }) -> Id =/= Name end, Users), 
	[ forward(Socket,U,Message) || U <- Recipients ],
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
	urelay_stats:add(room_bytes_in, size(Packet)),
	case member(IPAddr,Port,Bans) of
		true -> urelay_log:log(?MODULE,"Banned ~p:~p ~p~n", [ IPAddr,Port,Packet]);
		false -> false
	end,
	case member(IPAddr,Port,Users) of
		true -> 
			[ forward(Socket,U,Packet) || U <- except(IPAddr,Port,Users) ],
			[ relay(Socket,P,Packet) || P <- Peers];
		false -> false
	end,
	{ noreply, Room };

handle_info({udp_error, Socket, Error}, State) ->
	urelay_log:log(?MODULE,"socket ~p udp error ~p~n", [ Socket, Error ]),
	{ noreply, State };

handle_info( Message, Room ) ->
	urelay_log:log(?MODULE,"unknown message ~p~n", [ Message ]),
	{ noreply, Room }.

terminate(Reason, #room{ name = Name } ) ->
	urelay_log:log(?MODULE,"Shutting down ~p because ~p~n", [ Name, Reason ]),
	ok.

code_change( Version, Room, _Extra) ->
	urelay_log:log(?MODULE,"Loading ~p", [ Version ]),
	{ ok, Room }.	

send(Type,Stat,Socket,IPAddr,Port,Message) ->
	urelay_log:log(?MODULE,"sending to ~p:~p ~p~n", [ IPAddr, Port, Message ]),
	gen_udp:send(Socket,IPAddr,Port,Message),
	urelay_stats:increment(Type),
	urelay_stats:add(Stat,size(Message)).

forward(Socket, #user{ ipaddr = IPAddr, port = Port, filter_module =  Module, filter_function = Function }, Message ) ->
	Test = erlang:apply(Module,Function,[Message]) , 
	case Test of
		false ->
			urelay_log:log(?MODULE,"discarded message ~p", [ Message ]);
		_ ->
			send(room_broadcasts,room_boadcast_bytes_out,Socket,IPAddr,Port,Message)
	end.

relay(Socket, #peer{ ipaddr = IPAddr, port = Port }, Message ) ->
	send(room_replays,room_relay_bytes_out,Socket,IPAddr,Port,Message).

member(IPAddr,Port,Users) ->
	[] /= lists:filtermap( fun(#user{ ipaddr = I, port = P }) -> 
		(I =:= IPAddr) and (P =:= Port) end, Users).
	
except(IPAddr,Port,Users) ->
	lists:filtermap( fun(#user{ ipaddr = I, port = P }) ->
		(I /= IPAddr) or (P /= Port) end, Users ).

nofilter(_Message) ->
	true.
