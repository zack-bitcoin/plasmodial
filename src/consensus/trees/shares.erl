-module(shares).
%Each account has a tree of shares. 
%The shares are stored by share id. The id of a share determines it's difficulty. You can own either a negative, positive, or zero amount of each type of share. Shares are transferable
-export([test/0, change_amount/2, id/1, amount/1,
	 get/2, write/2]).
-record(share, {id, amount}).
-define(name, shares).

change_amount(S, A) ->
    S#share{amount = S#share.amount + A}.
id(X) -> X#share.id.
amount(X) -> X#share.amount.
new(ID, Amount) ->
    #share{id = ID, amount = Amount}.
serialize(X) ->
    A = X#share.amount,
    Sign = if
	       A>0 -> 1;
	       true -> 0
	   end,
    BAL = constants:balance_bits(),
    KL = constants:key_length()*8,
    <<Sign:8, A:BAL, (X#share.id):KL>>.
deserialize(X) ->
    BAL = constants:balance_bits(),
    KL = constants:key_length()*8,
    <<Sign:8, A:BAL, ID:KL>> = X,
    B = case Sign of
	    0 -> -A;
	    1 -> A
	end,
    #share{id = ID, amount = B}.
get(ID, Tree) ->
    {X, Leaf, Proof} = trie:get(ID, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {X, V, Proof}.
write(A, Tree) -> 
    ID = A#share.id,
    X = serialize(A),
    trie:put(ID, X, 0, Tree, ?name).
	    

test() ->
    Key = 1,
    C = new(Key, 100),
    {_, empty, _} = get(Key, 0),
    NewLoc = write(C, 0),
    {_, C, _} = get(Key, NewLoc),
    {_, empty, _} = get(Key, 0),
    success.
