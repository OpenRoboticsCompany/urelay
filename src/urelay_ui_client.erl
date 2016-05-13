-module(urelay_ui_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ accept/1 ]).

accept(Listen) ->
	{ ok, Socket } = gen_tcp:accept(Listen),
	gen_server:cast(ui,accept),
	loop(Socket).

loop(Socket) ->
	{ ok, { Address, Port }} = inet:peername(Socket),
	{ ok, Web } = file:read_file("ui.web"),
	receive
	{ tcp, Socket, Message } ->
		io:format("~p:~p ~p~n", [ Address, Port, Message ]),
		gen_tcp:send(Socket,Web),
		loop(Socket); { tcp_closed, Socket } -> io:format("~p:~p closed~n", [ Address, Port ]);
	{ error, closed } ->
		io:format("~p closing~n", [ Socket ]); 
	Message -> 
		io:format("unknown message ~p~n", [ Message ])
	end.
		
		
