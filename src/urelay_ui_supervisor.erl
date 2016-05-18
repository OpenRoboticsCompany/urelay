-module(urelay_ui_supervisor).
-behavior(supervisor).
-export([ start_link/0, init/1]).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, []).


init([]) ->
	{ ok, { #{ strategy => one_for_one, intensity => 1, period => 10}, [ 
	#{ 
		id => ui,
		start => { urelay_ui, start_link, [ 8880 ] },
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ urelay_ui, urelay_ui_client ] },
	#{ 
		id => ui_room,
		start => { urelay_room, start_link, [ ui_room, 5678 ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		module => [ urelay_room ] },
	#{
		id => urelay_websocket,
		start => { urelay_websocket, start_link, [ 
			{{127,0,0,1},5678}, 8888, 5677 ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		module => [ urelay_websocket, urelay_websocket_supervisor,urelay_room, websocket, websocket_rfc6455, websocket_server ] }
	]}}.

