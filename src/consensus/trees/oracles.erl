-module(oracles).
-export([new/4,write/2,get/2,id/1,result/1,
	 question/1,starts/1,root_hash/1, 
	 type/1, test/0]).
-define(name, oracles).
-record(oracle, {id, 
		 result, 
		 question, 
		 starts, 
		 type, %0 means oracle is live. 1 means it returned true, 2 means it returned false, 3 means that it was a bad question.
		 orders, 
		 creator}).
%we need to store a pointer to the orders tree in the meta data.

id(X) -> X#oracle.id.
result(X) -> X#oracle.result.
question(X) -> X#oracle.question.
starts(X) -> X#oracle.starts.
type(X) -> X#oracle.type.
new(ID, Question, Starts, Creator) ->
    Orders = orders:empty_book(),
    %Orders = OrdersTree,
    #oracle{id = ID,
	    result = 0,
	    question = Question,
	    starts = Starts,
	    type = 0,
	    orders = Orders,
	    creator = Creator
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
    HEI = constants:height_bits(),
    <<(X#oracle.id):KL,
      (X#oracle.result):8,
      (X#oracle.type):8,
      (X#oracle.starts):HEI,
      (X#oracle.creator):KL,
      Question/binary,
      Orders/binary>>.
deserialize(X) ->
    KL = constants:key_length(),
    HS = constants:hash_size()*8,
    HEI = constants:height_bits(),
    <<ID:KL,
      Result:8,
      Type:8,
      Starts:HEI,
      Creator:KL,
      Question:HS,
      _Orders:HS
    >> = X,
    #oracle{
       id = ID,
       type = Type,
       result = Result,
       starts = Starts,
       question = <<Question:HS>>,
       creator = Creator
      }.
write(Oracle, Root) ->
    %meta is a pointer to the orders tree.
    V = serialize(Oracle),
    Key = Oracle#oracle.id,
    Meta = Oracle#oracle.orders,
    io:fwrite("write meta "),
    io:fwrite(integer_to_list(Meta)),
    io:fwrite("\n"),
    trie:put(Key, V, Meta, Root, ?name).
get(ID, Root) ->
    {RH, Leaf, Proof} = trie:get(ID, Root, ?name),
    V = case Leaf of 
	    empty -> empty;
	    L -> 
		X = deserialize(leaf:value(L)),
		M = leaf:meta(L),
		io:fwrite("get meta "),
		io:fwrite(integer_to_list(M)),
		io:fwrite("\n"),
		X#oracle{orders = M}
	end,
    {RH, V, Proof}.


test() ->
    Root = 0,
    X = new(1, testnet_hasher:doit(1), 2, 1),
    X2 = deserialize(serialize(X)),
    X = X2#oracle{orders = X#oracle.orders},
    NewLoc = write(X, Root),
    {_, X3, _} = get(X#oracle.id, NewLoc),
    %io:fwrite({X, X3}),
    {_, empty, _} = get(X#oracle.id, 0),
    success.
    
