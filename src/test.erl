-module(test).
-export([ test/0 ]).

-record(user, { ipaddr, port, name }).

test() ->
	urelay_ui_supervisor:start_link(),
	urelay_room:join(ui_room, #user{ ipaddr = { 127,0,0,1}, port = 5677 }),
	urelay_room:join(ui_room, #user{ ipaddr = { 127,0,0,1}, port = 5679 }),
	ujson_server:start_link(5679, ujson_echo, echo).
	
	
