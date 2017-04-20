-module(oracles).
-export([new/5,write/2,get/2,id/1,result/1,
	 question/1,starts/1,root_hash/1, 
	 type/1, difficulty/1, orders/1,
	 set_orders/2, done_timer/1, set_done_timer/2,
	 set_result/2, set_type/2,
	 test/0]).
-define(name, oracles).
-record(oracle, {id, 
		 result, 
		 question, 
		 starts, 
		 type, %0 means order book is empty, 1 means the order book is holding shares of true, 2 means it holds false, 3 means that it holds shares of "bad question".
		 orders, 
		 creator,
		 difficulty,
		 done_timer}).
%we need to store a pointer to the orders tree in the meta data.

id(X) -> X#oracle.id.
result(X) -> X#oracle.result.
question(X) -> X#oracle.question.
starts(X) -> X#oracle.starts.
type(X) -> X#oracle.type.
difficulty(X) -> X#oracle.difficulty.
orders(X) -> X#oracle.orders.
done_timer(X) -> X#oracle.done_timer.
set_orders(X, Orders) ->
    X#oracle{orders = Orders}.
set_done_timer(X, H) ->
    X#oracle{done_timer = H}.
set_result(X, R) ->
    X#oracle{result = R}.
set_type(X, T) ->
    true = is_integer(T),
    true = T > -1,
    true = T < 5,
    X#oracle{type = T}.
new(ID, Question, Starts, Creator, Difficulty) ->
    Orders = orders:empty_book(),
    %Orders = OrdersTree,
    #oracle{id = ID,
	    result = 0,
	    question = Question,
	    starts = Starts,
	    type = 0,
	    orders = Orders,
	    creator = Creator,
	    difficulty = Difficulty,
	    done_timer = Starts + constants:minimum_oracle_time()
	   }.
root_hash(Root) ->
    trie:root_hash(?name, Root).
serialize(X) ->
    KL = constants:key_length(),
    HS = constants:hash_size(),
    Question = X#oracle.question,
    Orders = orders:root_hash(X#oracle.orders),
    %Orders = X#oracle.orders,
    HS = size(Question),
    HS = size(Orders),
    HB = constants:height_bits(),
    DB = constants:difficulty_bits(),
    <<(X#oracle.id):KL,
      (X#oracle.result):8,
      (X#oracle.type):8,
      (X#oracle.starts):HB,
      (X#oracle.creator):KL,
      (X#oracle.difficulty):DB,
      (X#oracle.done_timer):HB,
      Question/binary,
      Orders/binary>>.
deserialize(X) ->
    KL = constants:key_length(),
    HS = constants:hash_size()*8,
    HEI = constants:height_bits(),
    DB = constants:difficulty_bits(),
    <<ID:KL,
      Result:8,
      Type:8,
      Starts:HEI,
      Creator:KL,
      Diff:DB,
      DT:HEI,
      Question:HS,
      _Orders:HS
    >> = X,
    #oracle{
       id = ID,
       type = Type,
       result = Result,
       starts = Starts,
       question = <<Question:HS>>,
       creator = Creator,
       difficulty = Diff,
       done_timer = DT
      }.
write(Oracle, Root) ->
    %meta is a pointer to the orders tree.
    V = serialize(Oracle),
    Key = Oracle#oracle.id,
    Meta = Oracle#oracle.orders,
    trie:put(Key, V, Meta, Root, ?name).
get(ID, Root) ->
    {RH, Leaf, Proof} = trie:get(ID, Root, ?name),
    V = case Leaf of 
	    empty -> empty;
	    L -> 
		X = deserialize(leaf:value(L)),
		M = leaf:meta(L),
		X#oracle{orders = M}
	end,
    {RH, V, Proof}.


test() ->
    Root = 0,
    X = new(1, testnet_hasher:doit(1), 2, 1, constants:initial_difficulty()),
    X2 = deserialize(serialize(X)),
    X = X2#oracle{orders = X#oracle.orders},
    NewLoc = write(X, Root),
    {_, X, _} = get(X#oracle.id, NewLoc),
    %io:fwrite({X, X3}),
    {_, empty, _} = get(X#oracle.id, 0),
    success.
    
