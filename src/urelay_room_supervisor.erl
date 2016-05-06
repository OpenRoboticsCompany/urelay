-module(urelay_room_supervisor).
-behavior(supervisor).
-export([ start_link/0, init/1, start_room/3, stop_room/2 ]).

start_link() ->
	supervisor:start_link(?MODULE,[]).

init(_Args) ->
	{ok, { #{ strategy => one_for_one, intensity => 10, period => 10}, []}}.

start_room(Self,Name,Port) ->
	supervisor:start_child(Self, #{ 
		id => Name, 
		start => [ urelay_room, start_link, [ Name, Port ]],
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ urelay_room ]
	}).

stop_room(Self,Name) ->
	supervisor:terminate_child(Self,Name).
