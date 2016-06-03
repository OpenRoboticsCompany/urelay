-module(urelay_log).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/1, stop/0, log/3, log/2 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(urelay_log, { file, io }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public APU
%

start_link(File) ->
	gen_server:start_link({ local, ?MODULE}, ?MODULE, [File], []).

log(Module,Format,Args) ->
	gen_server:cast(?MODULE, { log, Module, Format, Args }).

log(Module,Format) ->
	gen_server:cast(?MODULE, { log, Module, Format, [] }).

stop() ->
	gen_server:call(?MODULE,stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private APU
%

init([File]) ->
	case file:open(File,[append]) of
		{ ok, IO } -> io:format("opened ~p for logging~n", [ File ]);
		Error -> io:format("failed to open ~p for logging ~p ~n", [ File, Error ]), IO = false
	end,	
	{ ok, #urelay_log{ file = File, io = IO }}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	log(?MODULE,"unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast({ log, Module, Format, Args }, State = #urelay_log{ io = IO }) ->
	io:format("[~p] " ++ Format, [ Module | Args ]),
	io:format(IO,"[~p] " ++ Format, [ Module | Args ]),
	{ noreply, State };

handle_cast(Message,State) ->
	log(?MODULE,"unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	log(?MODULE,"unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
