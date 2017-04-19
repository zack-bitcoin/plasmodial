-module(existence).
-export([get/2,write/2,new/1,hash2int/1,root_hash/1,hash/1, test/0]).
%for accessing the proof of existence tree
-record(exist, {hash}).
-define(name, existence).
hash(X) ->
    X#exist.hash.
new(Hash) ->
    HS = constants:hash_size(),
    HS = size(Hash),
    #exist{hash = Hash}.
serialize(E) ->
    HS = constants:hash_size(),
    Hash = E#exist.hash,
    HS = size(Hash),
    Hash.
deserialize(B) ->
    HS = constants:hash_size()*8,
    <<Hash:HS>> = B,
    #exist{hash = <<Hash:HS>>}.

get(Hash, Tree) ->
    true = is_binary(Hash),
    Key = hash2int(Hash),
    {X, Leaf, Proof} = trie:get(Key, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> 
		Y = leaf:value(L),
		deserialize(Y)
	end,
    {X, V, Proof}.
write(E, Tree) ->
    Hash = E#exist.hash,
    Key = hash2int(Hash),
    X = serialize(E),
    trie:put(Key, X, 0, Tree, ?name).
	     
hash2int(X) -> 
    S = size(X),
    S = constants:hash_size(),
    hash2int(X, 0).
hash2int(<<>>, N) -> N;
hash2int(<<X, Y/binary>>, N) ->
    M = (N*256) + X,
    hash2int(Y, M).
root_hash(Root) ->
    trie:root_hash(?name, Root).


test() ->
    Hash = testnet_hasher:doit(2),
    C = new(Hash),
    %C = testnet_hasher:doit(2),
    {_, empty, _} = get(Hash, 0),
    NewLoc = write(C, 0),
    C2 = new(testnet_hasher:doit(4)),
    NewLoc2 = write(C2, NewLoc),
    {_, C, _} = get(Hash, NewLoc2),
    {_, empty, _} = get(Hash, 0),
    success.
