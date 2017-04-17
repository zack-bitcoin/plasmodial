-module(shares).
-export([write/2, delete/2,
	 get/2, test/0]).

%Each account has it's own tree of shares.
%Shares are queried by the id of the cooresponding oracle.
%Each oracle id can only be used for exactly one question.

-record(shares, {oracle_id = 0, %location in merkel tree.
		 amount1 = 0,%each oracle has 6 types of shares.
		 amount2 = 0,
		 amount3 = 0,
		 amount4 = 0,
		 amount5 = 0,
		 amount6 = 0}).
serialize(_S) ->
    ok.
deserialize(_S) ->
    ok.
write(_Leaf, _Root) ->
    ok.
delete(ID, Root) ->
    trie:delete(ID, Root, shares).
get(_ID, _Root) ->
    ok.

test() ->
    ok.
