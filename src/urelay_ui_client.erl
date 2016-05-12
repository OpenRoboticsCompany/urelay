-module(urelay_ui_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ accept/1 ]).

accept(Listen) ->
	{ ok, Socket } = gen_tcp:accept(Listen),
