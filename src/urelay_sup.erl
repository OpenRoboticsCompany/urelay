-module(urelay_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ ok, LOG_FILE } = application:get_env(urelay,log_file),
	{ ok, API_PORT } = application:get_env(urelay,api_port),
	{ok, { #{ strategy => one_for_one, intensity => 5,  period => 10}, [
	#{	
		id => urelay_log,
		start => { urelay_log, start_link, [ LOG_FILE ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ urelay_log ] },
	#{
		id => urelay_stats,
		start => { urelay_stats, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ urelay_stats ] },
	#{
		id => urelay_api,
		start => { urelay_api, start_link, [ API_PORT ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ urelay_stats ] },

	#{
		id => urelay_ui_supervisor,
		start => { urelay_ui_supervisor, start_link, [] },
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [ urelay_ui_supervisor ] }

	]} }.

