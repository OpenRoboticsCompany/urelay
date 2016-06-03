-module(urelay_ui_supervisor).
-behavior(supervisor).
-export([ start_link/0, init/1]).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, []).


init([]) ->
	{ ok, UI_HTTP_PORT } = application:get_env(urelay,ui_http_port),
	{ ok, UI_ROOM_IPADDR } = application:get_env(urelay,ui_room_ipaddr),
	{ ok, UI_ROOM_PORT } = application:get_env(urelay,ui_room_port),
	{ ok, UI_WEBSOCKET_PORT } = application:get_env(urelay,ui_websocket_port),
	{ ok, UI_RELAY_PORT } = application:get_env(urelay,ui_relay_port),
	{ ok, { #{ strategy => one_for_one, intensity => 1, period => 10}, [ 
	#{ 
		id => ui,
		start => { urelay_ui, start_link, [ UI_HTTP_PORT ] },
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ urelay_ui, urelay_ui_client ] },
	#{ 
		id => ui_room,
		start => { urelay_room, start_link, [ ui_room, UI_ROOM_PORT ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		module => [ urelay_room ] },
	#{
		id => urelay_websocket,
		start => { urelay_websocket, start_link, [ 
			{ ui_room, UI_ROOM_IPADDR, UI_ROOM_PORT}, 
			UI_WEBSOCKET_PORT, 
			UI_RELAY_PORT ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		module => [ urelay_websocket, urelay_websocket_supervisor,urelay_room, websocket, websocket_rfc6455, websocket_server ] }
	]}}.

