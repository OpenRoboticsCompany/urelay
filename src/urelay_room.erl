-module(urelay_room).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/2, 
	join/2, leave/2, ban/2,
	announce/2, message/3, broadcast/2, 
	list/1, close/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(user_id, { ipaddr, port, name }).
-record(room, { name, users, bans }).

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

announce(Room,Id) ->
	gen_server:call(Room, { announce, Id }).

message(Room,Id,Message) ->
	gen_server:call(Room, { message, Id, Message }).

broadcast(Room,Message) ->
	gen_server:call(Room, { broadcast, Message }).

list(Room) ->
	gen_server:call(Room, list).

close(Room) ->
	gen_server:call(Room, close).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([ Name, Port ]) ->
	io:format("Starting room ~p~n", [ Name ]),
	{ ok, #room{ name = Name, users = [], bans = [] }}.

handle_call( { join, Id }, _From, Room = #room{ users = Users, bans = Bans }) ->
	case lists:member( Id, Bans ) of
		true ->
			{ reply, { banned, Id }, Room };	
		false ->	
			{ reply, ok, Room#room{ users = [ Id | Users ] } }
	end;

handle_call( { leave, Id }, _From, Room = #room{ users = Users }) ->
	{ reply, ok, Room#room{ users = lists:delete(Id,Users) }};

handle_call( { ban, Id }, _From, Room = #room{ users = Users, bans = Bans }) ->
	{ reply, ok, Room#room{ 
		users = lists:delete(Id,Users),
		bans = [ Id | Bans ] }};

handle_call( { announce, Id }, _From, Room = #room{ users = Users }) ->
	{ reply, ok, Room };

handle_call( { message, Id, Message }, _From, Room = #room{ users = Users }) ->
	{ reply, ok, Room };

handle_call( { broadcast, Message }, _From, Room = #room{ users = Users }) ->
	{ reply, ok, Room };

handle_call( list, _From, Room = #room{ users = Users }) ->
	{ reply, ok, Room };

handle_call( close, _From, Room ) ->
	{ reply, ok, Room };

handle_call( _Message, _From, Room ) ->
	{ reply, ok, Room }.

handle_cast( _Message, Room ) ->
	{ noreply, Room }.

handle_info( _Message, Room ) ->
	{ noreply, Room }.

terminate(Reason, Room = #room{ name = Name } ) ->
	io:format("Shutting down ~p because ~p~n", [ Name, Reason ]),
	ok.

code_change( _Old, Room, _Extra) ->
	{ ok, Room }.	


