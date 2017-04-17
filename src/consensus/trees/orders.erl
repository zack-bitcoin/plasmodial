-module(orders).
-export([match/2, add/2, root_hash/1, id/1, amount/1,
	 pointer/1, new/3, get/2, empty_book/0,
	 remove/2,
	 test/0]).
-define(name, orders).
-record(order, {id, aid, amount, pointer}).
id(X) -> X#order.id.
amount(X) -> X#order.amount.
pointer(X) -> X#order.pointer.
update_pointer(X, P) ->
    X#order{pointer = P}.
update_amount(X, A) ->
    B = X#order.amount + A,
    true = B>0,
    X#order{amount = B}.
	    
new(ID, AID, Amount) ->
    true = ID > 1,
    #order{id = ID, aid = AID, amount = Amount, pointer = 0}.
serialize_head(X) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    Y = OL+OL+KL+BAL,
    <<X:Y>>.
deserialize_head(X) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    Y = OL+OL+KL+BAL,
    <<Z:Y>> = X,
    Z.
serialize(A) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    <<(A#order.id):OL,
      (A#order.aid):KL,
      (A#order.amount):BAL,
      (A#order.pointer):OL>>.
deserialize(B) ->
    KL = constants:key_length(),
    OL = constants:orders_bits(),
    BAL = constants:balance_bits(),
    <<ID:OL, AID:KL,
      Amount:BAL, P:OL>> = B,
    #order{id = ID, aid = AID, amount = Amount,
	   pointer = P}.
write(X, Root) -> 
    V = serialize(X),
    Key = id(X),
    trie:put(Key, V, 0, Root, ?name).
get(ID, Root) ->
    {RH, Leaf, Proof} = trie:get(ID, Root, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
empty_book() ->
    X = serialize_head(0),
    trie:put(1, X, 0, 0, ?name).
head_get(Root) ->
    {_, L, _} = trie:get(1, Root, ?name),
    deserialize_head(leaf:value(L)).
head_put(X, Root) ->
    Y = serialize_head(X),
    trie:put(1, Y, 0, Root, ?name).
add(Order, Root) ->
    X = id(Order),
    {_, empty, _} = get(X, Root),
    %make the end of the list point to the new order.
    P = head_get(Root),
    case P of
	0 ->
	    Root2 = head_put(X, Root),
	    write(Order, Root2);
	Y ->
	    add2(Order, Root, Y)
    end.
add2(Order, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#order.pointer,
    case N of
	0 ->
	    L2 = update_pointer(L, id(Order)),
	    Root2 = write(L2, Root),
	    0 = Order#order.pointer,
	    write(Order, Root2);
	M ->
	    add2(Order, Root, M)
    end.
remove(ID, Root) ->
    T = head_get(Root),
    {_,Order,_} = get(T, Root),
    Q = Order#order.id,
    if 
	ID == Q -> 
	    head_put(Q#order.pointer, Root);
	true ->
	    remove2(ID, Root, T)
    end.
remove2(ID, Root, P) ->
    {_, L, _} = get(P, Root),
    N = L#order.pointer,
    case N of
	ID ->
	    {_, L2, _} = get(ID, Root),
	    N = update_pointer(L, id(L2)),
	    Root2 = delete(N, Root),
	    write(N, Root2);
	X -> remove2(ID, Root, X)
    end.
delete(ID, Root) ->
    trie:delete(ID, Root, ?name).
match(Order, Root) ->
    T = head_get(Root),
    match2(Order, Root, T).
match2(Order, Root, T) ->
    {_, L, _} = get(T, Root),
    OldA = L#order.amount,
    NewA = Order#order.amount,
    P = L#order.pointer,
    if
	NewA > OldA ->
	    Root2 = head_put(P, Root),
	    Order2 = update_amount(Order, -OldA),
	    Root3 = delete(id(L), Root2),
	    match2(Order2, Root3, P);
	NewA == OldA ->
	    head_put(P, Root);
	NewA < OldA ->
	    Order2 = update_amount(L, -NewA),
	    write(Order2, Root)
    end.

root_hash(Root) ->
    trie:root_hash(?name, Root).

test() ->
    Root0 = empty_book(),
    ID1 = 5,
    ID2 = 3,
    Order1 = new(ID1, 2, 100),
    Order2 = new(ID2, 2, 100),
    Root1 = add(Order1, Root0),
    Root2 = add(Order2, Root1),
    Order3 = new(6, 5, 110),
    Root3 = match(Order3, Root2),
    {_, empty, _} = get(ID1, 0),
    {_, {order, 5, 2, 100, _}, _} = get(ID1, Root2),
    {_, {order, 3, 2, 100, _}, _} = get(ID2, Root2),
    {_, empty, _} = get(ID1, Root3),
    success.
    
