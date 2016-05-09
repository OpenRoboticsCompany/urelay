-module(urelay_websocket_supervisor).
-behavior(supervisor).
-export([ start_link/1, init/1]).

start_link(WSPort) ->
	supervisor:start_link( { local, ?MODULE}, ?MODULE,[WSPort]).

init([WSPort]) ->
	{ok, { #{ strategy => one_for_one, intensity => 1, period => 10}, [ #{ 
		id => list_to_atom("websocket_server_" ++ integer_to_list(WSPort)), 
		start => { websocket_server, start_link, [ urelay_websocket, relay, WSPort ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ websocket_server, websocket, websocket_rfc6455 ]
	}]}}.
