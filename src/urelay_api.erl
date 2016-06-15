-module(urelay_api).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/1, stop/0, rooms/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-include("urelay.hrl").
-compile([debug_info]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Port) ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, #api{ port = Port }, []).

stop() ->
	gen_server:call(?MODULE,stop).

rooms() ->
	gen_server:call(?MODULE,rooms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(API = #api{ port = Port }) ->
	urelay_log:log(?MODULE,"Starting api on port ~p~n", [ Port ]),
	{ ok, Socket } = gen_udp:open(Port,[ binary, { active, true }]),
	{ ok, Super } = urelay_room_supervisor:start_link(),
	{ ok, API#api{ socket = Socket, rooms = [], supervisor = Super } }.

handle_call(stop,_From,API) ->
	{ stop, ok, API };

handle_call(rooms,_From,API = #api{ rooms = Rooms }) ->
	{ reply, { ok, Rooms }, API };

handle_call(Message, _From, API) ->
	io:format("Got message ~p~n", [ Message ]),
	{ reply, ok, API }.

handle_cast(Message,API) ->
	io:format("Got cast ~p~n", [ Message ]),
	{ noreply, API }.

handle_info({ udp, _Client, IPAddr, Port, Packet }, API) ->
	urelay_log:log(?MODULE,"Got packet ~p~n", [ Packet ]),
	urelay_stats:increment(api_packets_in),
	urelay_stats:add(api_bytes_in,size(Packet)),
	{ Command, _Rem } = ujson:decode(Packet),
	urelay_log:log(?MODULE,"Got command ~p from ~p:~p ~n", [ Command, IPAddr, Port ]),
	urelay_stats:increment(api_calls),
	dispatch(API,IPAddr,Port,Command);

handle_info(Message,API) ->
	urelay_log:log(?MODULE,"Got message ~p~n", [ Message ]),
	{ noreply, API }.	

terminate(Reason,API = #api{ socket = Socket }) ->
	urelay_log:log(?MODULE,"Stopping ~p because ~p~n", [ API, Reason ]),
	gen_udp:close(Socket),
	ok.

code_change(_Old,_Vsn,API) ->
	{ ok, API }.

dispatch(API = #api{ supervisor = Super, rooms = Rooms, socket = Socket },IPAddr,Port,[ Command | Args ]) ->
	case Command of
		"start" -> 
			[ RoomName, RoomPort ] = Args,
			urelay_room_supervisor:start_room(Super,RoomName,RoomPort),
			urelay_stats:increment(rooms),
			Message = ujson:encode([ "started", RoomName ]),
			gen_udp:send(Socket,IPAddr,Port, Message),	
			urelay_stats:add(api_bytes_out,size(Message)), 
			{ noreply, API#api{ rooms = [ RoomName | Rooms ] }};
		"stop" -> 
			[ RoomName ] = Args,
			urelay_room_supervisor:stop_room(Super,RoomName),
 			Message = ujson:encode([ "stoped", RoomName ]),
			gen_udp:send(Socket,IPAddr,Port,Message),	
			urelay_stats:add(api_bytes_out,size(Message)), 
			urelay_stats:decrement(rooms),
			{ noreply, API#api{ rooms = lists:delete(RoomName,Rooms) }};
		"rooms" -> 
			Message = ujson:encode([ "rooms", Rooms ]),
			gen_udp:send(Socket,IPAddr,Port,Message),	
			urelay_stats:add(api_bytes_out,size(Message)),
			{ noreply, API };
		"join" ->
			[ Room, IP, Port, FilterModule, FilterFunction, FilterArgs] = Args,
			urelay_room:join(Room, IP, Port, FilterModule, FilterFunction, FilterArgs),
			urelay_stats:increment(api_room_joins),
			{ noreply, API };
		_ -> 
			urelay_log:log(?MODULE,"unknown command ~p~n", [ Command ]),
			urelay_stats:increment(api_unknown_command),
			{ noreply, API }
	end.
