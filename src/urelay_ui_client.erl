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
	urelay_log:log(?MODULE,"connection ~p:~p~n", [ Address, Port ]),
	{ ok, Web } = file:read_file("ui.web"),
	receive
	{ tcp, Socket, Message } ->
		urelay_log:log(?MODULE,"~p:~p ~p~n", [ Address, Port, Message ]),
		urelay_stats:add(ui_client_bytes_in, size(Message)),	
		Length = integer_to_binary(size(Web)),
		Response = <<"HTTP/1.1 200 OK\nContent-Type: text/html\nConnection:close\nContent-Length: ", Length/binary, "\n\n", Web/binary >>,
		gen_tcp:send(Socket,Response),
		urelay_stats:add(ui_client_bytes_out, size(Response)),
		loop(Socket); 
	{ tcp_closed, Socket } -> 
		urelay_log:log(?MODULE,"~p:~p closed~n", [ Address, Port ]);
	{ error, closed } ->
		urelay_log:log(?MODULE,"~p closing~n", [ Socket ]); 
	Message -> 
		urelay_log:log(?MODULE,"unknown message ~p~n", [ Message ])
	end.
