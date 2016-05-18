-module(urelay_ui_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ accept/1 ]).

accept(Listen) ->
	case gen_tcp:accept(Listen) of
		{ ok, Socket } -> 
			gen_server:cast(ui,accept),
			loop(Socket);
		{ error, closed } ->
			io:format("closing client~n")
	end.

loop(Socket) ->
	{ ok, { Address, Port }} = inet:peername(Socket),
	{ ok, Web } = file:read_file("ui.web"),
	receive
	{ tcp, Socket, Message } ->
		io:format("~p:~p ~p~n", [ Address, Port, Message ]),
		Length = integer_to_binary(size(Web)),
		gen_tcp:send(Socket,<<"HTTP/1.1 200 OK\nContent-Type: text/html\nConnection:close\nContent-Length: ", Length/binary, "\n\n", Web/binary >>),
		loop(Socket); { tcp_closed, Socket } -> io:format("~p:~p closed~n", [ Address, Port ]);
	{ error, closed } ->
		io:format("~p closing~n", [ Socket ]); 
	Message -> 
		io:format("unknown message ~p~n", [ Message ])
	end.
		
		
