-module(testnet_sup).
-behaviour(supervisor).
-export([start_link/0,init/1,stop/0]).%,start_http/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
-define(keys, [port, keys, 
	       block_hashes, top, block_absorber,
	       tx_pool, peers, tx_pool_feeder, 
	       mine, channel_manager, channel_feeder]).

child_maker([]) -> [];
child_maker([H|T]) -> [?CHILD(H, worker)|child_maker(T)].
child_killer([]) -> [];
child_killer([H|T]) -> 
    supervisor:terminate_child(testnet_sup, H),
    child_killer(T).
stop() -> 
    child_killer(?keys),
    halt().
%exit(keys, kill).
%supervisor:terminate_child(testnet_sup, keys).
tree_child(Id, KeySize, Size) ->
    tree_child(Id, KeySize, Size, 0).
tree_child(Id, KeySize, Size, Meta) ->
    Amount = constants:trie_size(),
    Sup = list_to_atom(atom_to_list(Id) ++ "_sup"),
    {Sup, {trie_sup, start_link, [KeySize, Size, Id, Amount, Meta, constants:hash_size(), hd]}, permanent, 5000, supervisor, [trie_sup]}.
init([]) ->
    %Amount = constants:trie_size(),
    KeyLength = constants:key_length(), 
    %FullLength = trie_hash:hash_depth()*2,
    HashSize = constants:hash_size(),
    FullLength = HashSize*8,
    Children = child_maker(?keys),
    Tries = [
	     tree_child(accounts, KeyLength, constants:account_size()),
	     tree_child(channels, KeyLength, constants:channel_size()),
	     tree_child(existence, FullLength, HashSize),
	     tree_child(oracles, KeyLength, ((constants:key_length() div 8) + 2 + (constants:height_bits() div 8) + (2*constants:hash_size())), (constants:key_length() div 8)),
	     tree_child(orders, KeyLength, ((constants:key_length() + (constants:orders_bits()*2) + constants:balance_bits()) div 8)),
	     tree_child(burn, FullLength, (constants:balance_bits() div 8) + constants:hash_size()),
	     tree_child(oracle_bets, KeyLength, (constants:key_length() + (constants:balance_bits() div 8))),
	     tree_child(shares, KeyLength, (constants:key_length() + 1 + (constants:balance_bits() div 8)))
	    ],
    {ok, { {one_for_one, 50000, 1}, Tries ++ Children} }.

