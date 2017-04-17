-module(oracle_bets).
-export([test/0, new/2, increase/2, id/1, amount/1, 
	 write/2, get/2]).
%Each account has a tree of oracle bets. Oracle bets are not transferable. Once an oracle is settled, the bets in it can be converted to shares.
-record(bet, {id, amount}).
-define(name, oracle_bets).
id(X) ->
    X#bet.id.
amount(X) ->
    X#bet.amount.
increase(X, A) ->
    X#bet{amount = X#bet.amount + A}.
new(OracleID, Amount) ->
    %{_, X, _} = active_oracles:read(OracleID, AORoot),
    %false = X == empty,
    #bet{id = OracleID, amount = Amount}.
serialize(X) ->
    KL = constants:key_length()*8,
    BAL = constants:balance_bits(),
    <<(X#bet.id):KL,
      (X#bet.amount):BAL>>.
deserialize(B) ->
    KL = constants:key_length()*8,
    BAL = constants:balance_bits(),
    <<ID:KL, A:BAL>> = B,
    #bet{amount = A, id = ID}.
write(X, Tree) ->
    Key = X#bet.id,
    Z = serialize(X),
    trie:put(Key, Z, 0, Tree, ?name).
get(ID, Tree) ->
    {X, Leaf, Proof} = trie:get(ID, Tree, ?name),
    V = case Leaf of 
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {X, V, Proof}.
test() ->
    C = new(1, 100),
    ID = C#bet.id,
    {_, empty, _} = get(ID, 0),
    Root = write(C, 0),
    {_, C, _} = get(ID, Root),
    {_, empty, _} = get(ID, 0),
    success.
    
    
