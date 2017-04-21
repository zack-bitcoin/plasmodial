-module(shares).
%Each account has a tree of shares. 
%The shares are stored by share id. The id of a share determines it's difficulty. You can own either a negative, positive, or zero amount of each type of share. Shares are transferable
-export([test/0, change_amount/2, id/1, amount/1,
	 get/2, write/3, root_hash/1, new/3,
	 receive_shares/3, send_shares/3,
	 write_many/3, write_many/2,
	 to_code/1, from_code/1]).
-record(share, {id, amount, 
	       modified}).%we need to keep a record of when it was modified so that users can get paid for having shares.
-define(name, shares).

to_code(Shares) ->
    S = "macro [ nil ;\n"
	++ "macro , swap cons ;\n"
	++ "macro ] swap cons reverse ;\n"
	++ "[ " ++ to_code2(Shares),
    list_to_binary(S).
to_code2([]) -> " ]\n";
to_code2([H]) -> to_code3(H) ++ " ]\n ";
to_code2([H|T]) -> to_code3(H) ++ " , " ++ to_code2(T).
to_code3(Share)-> 
    "[int " ++ integer_to_list(Share#share.id) ++", " ++
    "int " ++ integer_to_list(Share#share.amount) ++", " ++
    "int " ++ integer_to_list(Share#share.modified) ++"]".
from_code([]) -> [];
from_code([[ID, Amount, M]|T]) -> 
    <<I:32>> = ID,
    <<A:32>> = Amount,
    <<N:32>> = M,
    [#share{id = I, amount = A, modified = N}|
     from_code(T)].
    
change_amount(S, A) ->
    S#share{amount = S#share.amount + A}.
id(X) -> X#share.id.
amount(X) -> X#share.amount.
modified(X) -> X#share.modified.
set_modified(X, M) ->
    X#share{modified = M}.
new(ID, Amount, Height) ->
    #share{id = ID, amount = Amount, modified = Height}.
serialize(X) ->
    A = X#share.amount,
    HEI = constants:height_bits(),
    {Sign, A2} = if
	       A>0 -> {1, A};
	       true -> {0, -A}
	   end,
    BAL = constants:balance_bits(),
    KL = constants:key_length()*8,
    <<Sign:8, 
      A2:BAL, 
      (X#share.id):KL,
      (X#share.modified):HEI
    >>.
deserialize(X) ->
    BAL = constants:balance_bits(),
    KL = constants:key_length()*8,
    HEI = constants:height_bits(),
    <<Sign:8, A:BAL, ID:KL, M:HEI>> = X,
    B = case Sign of
	    0 -> -A;
	    1 -> A
	end,
    #share{id = ID, amount = B, modified = M}.
get(ID, Tree) ->
    {X, Leaf, Proof} = trie:get(ID, Tree, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {X, V, Proof}.
%make_tree_hash(S) ->
%    make_tree_hash(S, 0).
%make_tree_hash([], Tree) -> Tree;
%make_tree_hash([H|T], Tree) -> 
%    write2(H, Tree).
write_many([], Tree) -> Tree;
write_many([S|T], Tree) -> 
    Tree2 = write2(S, Tree),
    write_many(T, Tree2).
write_many([], Tree, _) -> Tree;
write_many([S|T], Tree, Old) -> 
    {_, SO, _} = get(id(S), Old),
    S2 = set_modified(S, modified(SO)),
    Tree2 = write2(S2, Tree),
    write_many(T, Tree2, Old).
write(A, Tree, Height) -> 
    B = A#share{modified = Height},
    write2(B, Tree).
write2(B, Tree) ->
    ID = B#share.id,
    X = serialize(B),
    trie:put(ID, X, 0, Tree, ?name).
delete(ID, Tree) ->
    trie:delete(ID, Tree, ?name).
flip_shares([]) -> [];
flip_shares([S|T]) -> 
    [S#share{amount = -S#share.amount}|
     flip_shares(T)].
send_shares(Shares, Trees, Height) ->
    receive_shares(flip_shares(Shares), Trees, Height).
receive_shares(S, T, H) ->
    receive_shares(S, T, H, 0).
receive_shares([], Tree, _, Tokens) -> {Tokens, Tree};
receive_shares([Share|S], Tree, Height, Tokens1) ->
    {Tokens2, Tree2} = receive_share(Share, Tree, Height),
    receive_shares(S, Tree2, Height, Tokens1+Tokens2).
receive_share(Share, Tree, Height) ->
    SID = id(Share),
    SA = Share#share.amount,
    {_, Old, _} = get(SID, Tree),
    if
	(Old == empty) and (0 == SA) ->
	    {0, Tree};
	Old == empty ->
	    false = 0 == SA,
	    {0, write(Share, Tree, Height)};
	true ->
	    {Shares, Tokens} = get_paid(Old, Height),
	    NewAmount = Shares + SA,
	    if
		NewAmount == 0 -> 
		    {Tokens, delete(SID, Tree)};
		true ->
		    X = Old#share{amount = NewAmount},
		    {Tokens, write(X, Tree, Height)}
	    end
    end.
diff(ID) -> ID.%+constants:initial_difficulty().
%diff_old(ID) ->
    %Base = constants:initial_difficulty(),
    %DiffFrac = {101, 100},%so every new difficulty is 1% higher
    %expt(Base, DiffFrac, ID).
%expt(Base, {T, B}, ID) ->
    %calculates the exponential of fraction T/B deterministically in log2(ID) steps
%    A = ID rem 2,
%    case A of
%	0 ->
%	    C = expt(Base, {T, B}, ID div 2),
%	    C*C;
%	1 -> expt(((Base * T) div B), {T, B}, ID-1)
%    end.
	    
get_paid(Share, Height) ->
    OldHeight = Share#share.modified,
    %for every height in the range, get the difficulty.
    SDiff = diff(Share#share.id),
    get_paid2(OldHeight, Height, SDiff, Share#share.amount, 0).
    %for every difficulty in the range, see if we are over or under. positive shares get paid for being over, negative get paid for being under.
get_paid2(Start, Start, _, Shares, T) -> {Shares, T};
get_paid2(Step, End, Diff, Shares, Tokens) when Shares > 0 -> 
    M = constants:shares_conversion(Shares),
    BlockDiff = block:difficulty(block:read_int(Step)),
    T = if 
	    BlockDiff > Diff -> M;
	    true -> 0
	end,
    get_paid2(Step+1, End, Diff, Shares-M, Tokens+T).
    
    %constants:shares_conversion(Many). %this tells how many shares disappear on this block.

	    
root_hash(Root) ->
    true = is_integer(Root),
    trie:root_hash(?name, Root).
	    

test() ->
    Key = 1,
    C = new(Key, -100, 1),
    {_, empty, _} = get(Key, 0),
    NewLoc = write(C, 0, 1),
    {_, C, _} = get(Key, NewLoc),
    root_hash(NewLoc),
    {_, empty, _} = get(Key, 0),
    success.
