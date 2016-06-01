-module(urelay_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"(C) 2016 David J. Goehrig"/utf8>>).

-export([ start/1, stop/1, rooms/0, main/1 ]).

%% Dispatch function that processes args
main([Cmd|Args]) ->
	case Cmd of 
		"start" -> start(Args);
		"stop" -> stop(Args);
		"rooms" -> rooms()
	end.

start([Name,Port]) when is_list(Port) ->
	start([Name, list_to_integer(Port)]);
start([Name,Port]) ->
	evoke(ujson:encode([ "start", Name, Port ])).

stop([Name]) ->
	evoke(ujson:encode([ "stop", Name ])).
	
rooms() ->
	evoke(ujson:encode([ "rooms" ])).
	
evoke(Cmd) ->
	{ ok, Socket } = gen_udp:open(5677, [binary, { active, true }]),
	gen_udp:send(Socket,{127,0,0,1},5678, Cmd),
	receive 
		{ udp, _Socket, _IPAddr, _Port, Packet } ->
			{ Resp, _Rem } = ujson:decode(Packet),
			io:format("~p~n", [ Resp ])
	after	
		1000 ->
			io:format("server timeout~n")
	end,
	gen_udp:close(Socket).

