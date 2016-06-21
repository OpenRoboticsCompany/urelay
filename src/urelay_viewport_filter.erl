-module(urelay_viewport_filter).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ within/2, outside/2 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%
%

within(A,B) ->
	not outside(A,B).

outside([ Xa, Ya, Wa, Ha ],[ Xb, Yb, Wb, Hb ]) ->
	( Xa + Wa < Xb ) or
	( Xa > Xb + Wb ) or
	( Ya + Ha < Yb ) or
	( Ya > Yb + Hb ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

within_test() ->
	?assert(within([0,0,10,10],[10,10,10,10])),
	?assert(within([10,0,10,10],[10,10,10,10])),
	?assert(within([0,10,10,10],[10,10,10,10])),
	?assert(within([10,10,10,10],[10,10,10,10])),
	?assert(within([20,20,10,10],[10,10,10,10])),
	?assert(within([20,10,10,10],[10,10,10,10])),
	?assert(within([10,20,10,10],[10,10,10,10])),
	?assertNot(within([0,0,9,9],[10,10,10,10])),
	?assertNot(within([8,0,1,1],[10,10,10,10])),
	?assertNot(within([0,8,1,1],[10,10,10,10])),
	?assertNot(within([8,8,1,1],[10,10,10,10])),
	?assertNot(within([21,0,1,1],[10,10,10,10])),
	?assertNot(within([0,21,1,1],[10,10,10,10])),
	?assertNot(within([21,21,1,1],[10,10,10,10])).


outside_test() ->
	?assertNot(outside([0,0,10,10],[10,10,10,10])),
	?assertNot(outside([10,0,10,10],[10,10,10,10])),
	?assertNot(outside([0,10,10,10],[10,10,10,10])),
	?assertNot(outside([10,10,10,10],[10,10,10,10])),
	?assertNot(outside([20,20,10,10],[10,10,10,10])),
	?assertNot(outside([20,10,10,10],[10,10,10,10])),
	?assertNot(outside([10,20,10,10],[10,10,10,10])),
	?assert(outside([0,0,9,9],[10,10,10,10])),
	?assert(outside([8,0,1,1],[10,10,10,10])),
	?assert(outside([0,8,1,1],[10,10,10,10])),
	?assert(outside([8,8,1,1],[10,10,10,10])),
	?assert(outside([21,0,1,1],[10,10,10,10])),
	?assert(outside([0,21,1,1],[10,10,10,10])),
	?assert(outside([21,21,1,1],[10,10,10,10])).



-endif.
