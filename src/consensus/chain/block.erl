-module(block).
-export([hash/1,check2/1,test/0,mine_test/0,genesis/0,
	 make/3,mine/2,height/1,accounts/1,channels/1,
	 accounts_hash/1,channels_hash/1,
	 read/1,binary_to_file/1,block/1,prev_hash/2,
	 prev_hash/1,read_int/1,check1/1,pow_block/1,
	 mine_blocks/2, hashes/1, block_to_header/1,
	 median_last/2,
	 guess_number_of_cpu_cores/0
	]).

-record(block, {height, prev_hash, txs, channels, 
		accounts, mines_block, time, 
		difficulty,
		magic = constants:magic()}).%tries: txs, channels, census, 
-record(block_plus, {block, pow, accounts, channels, accumulative_difficulty = 0, prev_hashes = {}}).%The accounts and channels in this structure only matter for the local node. they are pointers to the locations in memory that are the root locations of the account and channel tries on this node.
%prev_hash is the hash of the previous block.
%this gets wrapped in a signature and then wrapped in a pow.
block_to_header(Block) ->
    Height = Block#block.height,
    PH = Block#block.prev_hash,
    Channels = Block#block.channels,
    Accounts = Block#block.accounts,
    Miner = Block#block.mines_block,
    Time = Block#block.time,
    Diff = Block#block.difficulty,
    Magic = Block#block.magic,
    %channels, accounts, miner, height, can be made into one merkle trie, which reduces the size of the header by more than half.
    Mid = <<Height:(constants:height_bits()),
	    Channels/binary,
	    Accounts/binary,
	    Miner:(constants:acc_bits())>>,
    HM = testnet_hasher:doit(Mid),
    true = size(PH) == 12,
    <<PH/binary,
      HM/binary,
      Time:(constants:time_bits()),
      Diff:(constants:difficulty_bits()),
      Magic:(constants:magic_bits())>>.
      
hashes(BP) ->
    BP#block_plus.prev_hashes.
   
%block({Block, _Pow}) ->
%    Block;
block(P) when element(1, P) == pow ->
    pow:data(P);
block(BP) when is_record(BP, block_plus) ->
    block(BP#block_plus.block);
block(B) when is_record(B, block) -> B.
pow_block(B) when element(1, B) == pow -> B;
pow_block(BP) when is_record(BP, block_plus) ->
    pow_block(BP#block_plus.block).

channels(Block) ->
    Block#block_plus.channels.
channels_hash(BP) when is_record(BP, block_plus) ->
    channels_hash(pow:data(BP#block_plus.block));
channels_hash(Block) -> Block#block.channels.
accounts(BP) ->
    BP#block_plus.accounts.
accounts_hash(BP) when is_record(BP, block_plus) ->
    accounts_hash(pow:data(BP#block_plus.block));
accounts_hash(Block) ->
    Block#block.accounts.
height(X) ->
    B = block(X),
    B#block.height.
prev_hashes(PH) ->
    H = height(read(PH)),
    prev_hashes([PH], H, 2).
prev_hashes([PH|Hashes], Height, N) ->
    NHeight = Height - N,
    if
	NHeight < 1 -> list_to_tuple(lists:reverse([PH|Hashes]));
	true ->
	    B = read_int(NHeight, PH),
	    prev_hashes([hash(B)|[PH|Hashes]], NHeight, N*2)
    end.

   
prev_hash(0, BP) ->
    prev_hash(BP);
prev_hash(N, BP) ->%N=0 should be the same as prev_hash(BP)
    element(N, BP#block_plus.prev_hashes).
prev_hash(X) -> 
    B = block(X),
    B#block.prev_hash.
hash(X) -> 
    testnet_hasher:doit(term_to_binary(block_to_header(block(X)))).
time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
genesis() ->
    %the pointer to an empty trie is 0.
    Address = constants:master_address(),
    ID = 1,
    First = account:new(ID, Address, constants:initial_coins(), 0),
    Accounts = account:write(0, First),
    AccRoot = account:root_hash(Accounts),
    ChaRoot = channel:root_hash(0),

    %Block = 
    %#block{height = 0,
	       %txs = [],
	       %channels = ChaRoot,
	       %accounts = AccRoot,
	       %mines_block = ID,
	       %time = 0,
	       %difficulty = constants:initial_difficulty()},
    %Block = {pow,{block,0,<<0:(8*hash:hash_depth())>>,[], ChaRoot, AccRoot,
    Block = {pow,{block,0,<<0:(8*constants:hash_size())>>,[], ChaRoot, AccRoot,
		  %<<1,223,2,81,223,207,12,158,239,5,219,253>>,
		  %<<108,171,180,35,202,56,178,151,11,85,188,193>>,
		  1,0,4080, constants:magic()},
	     4080,44358461744572027408730},
    #block_plus{block = Block, channels = 0, accounts = Accounts}.
    
absorb_txs(PrevPlus, MinesBlock, Height, Txs) ->
    OldAccounts = PrevPlus#block_plus.accounts,
    NewAccounts = 
	case MinesBlock of
	    -1 ->
		OldAccounts;
	    {ID, Address} -> %for miners who don't yet have an account.
		{_, empty, _} = account:get(ID, OldAccounts),
		%We should also give the miner the sum of the transaction fees.
		NM = account:new(ID, Address, constants:block_reward(), Height),
		account:write(OldAccounts, NM);
	    MB -> %If you already have an account.
		NM = account:update(MB, OldAccounts, constants:block_reward(), none, Height),
		account:write(OldAccounts, NM)
	end,
    txs:digest(Txs, 
	       PrevPlus#block_plus.channels,
	       NewAccounts,
	       Height).
    
make(PrevHash, Txs, ID) ->%ID is the user who gets rewarded for mining this block.
    ParentPlus = read(PrevHash),
    Parent = block(ParentPlus),
    %Parent = pow:data(ParentPlus#block_plus.block),
    Height = Parent#block.height + 1,
    MB = mine_block_ago(Height - constants:block_creation_maturity()),
    {NewChannels, NewAccounts} = absorb_txs(ParentPlus, MB, Height, Txs),
    CHash = channel:root_hash(NewChannels),
    AHash = account:root_hash(NewAccounts),
    NextDifficulty = next_difficulty(ParentPlus),
    #block_plus{
       block = 
	   #block{height = Height,
		  prev_hash = PrevHash,
		  txs = Txs,
		  channels = CHash,
		  accounts = AHash,
		  mines_block = ID,
		  time = time_now()-5,
		  difficulty = NextDifficulty},
       accumulative_difficulty = next_acc(ParentPlus, NextDifficulty),
       channels = NewChannels, 
       accounts = NewAccounts,
       prev_hashes = prev_hashes(PrevHash)
      }.
next_acc(Parent, ND) ->
    Parent#block_plus.accumulative_difficulty + pow:sci2int(ND).
    %We need to reward the miner the sum of transaction fees.
mine(BP, Times) when is_record(BP, block_plus) ->
    Block = BP#block_plus.block,
    case mine2(Block, Times) of
	false -> false;
	Pow -> BP#block_plus{pow = Pow}
    end.
mine2(Block, Times) ->
    Difficulty = Block#block.difficulty,
    Header = block_to_header(Block),
    Pow = pow:pow(Header, Difficulty, Times, constants:hash_size()),
    Pow.
%verify({Block, Pow}) ->
%    Difficulty = Block#block.difficulty,
%    true = pow:above_min(Pow, Difficulty, constants:hash_size()).
next_difficulty(ParentPlus) ->
    %Parent = pow:data(ParentPlus#block_plus.block),
    Parent = block(ParentPlus),
    Height = Parent#block.height + 1,
    RF = constants:retarget_frequency(),
    X = Height rem RF,
    OldDiff = Parent#block.difficulty,
    PrevHash = hash(ParentPlus),
    if
	Height == 1 -> constants:initial_difficulty(); 
	Height < (RF+1) -> OldDiff;
	X == 0 -> retarget(PrevHash, Parent#block.difficulty);
	true ->  OldDiff
    end.
median(L) ->
    S = length(L),
    F = fun(A, B) -> A > B end,
    Sorted = lists:sort(F, L),
    lists:nth(S div 2, Sorted).
    
retarget(PrevHash, Difficulty) ->    
    F = constants:retarget_frequency() div 2,
    {Times1, Hash2000} = retarget2(PrevHash, F, []),
    {Times2, _} = retarget2(Hash2000, F, []),
    M1 = median(Times1),
    M2 = median(Times2),
    Tbig = M1 - M2,
    T = Tbig div F,
    %io:fwrite([Ratio, Difficulty]),%10/2, 4096
    ND = pow:recalculate(Difficulty, constants:block_time(), max(1, T)),
    max(ND, constants:initial_difficulty()).
retarget2(Hash, 0, L) -> {L, Hash};
retarget2(Hash, N, L) -> 
    BP = read(Hash),
    B = block(BP),
    T = B#block.time,
    H = B#block.prev_hash,
    retarget2(H, N-1, [T|L]).
   
check1(BP) -> 
    %check1 makes no assumption about the parent's existance.
    BH = hash(BP),
    GH = hash(genesis()),
    if
	BH == GH -> {BH, 0};
	true ->
	    Block = block(BP),
	    %io:fwrite(packer:pack(Block)),
	    Difficulty = Block#block.difficulty,
	    true = Difficulty >= constants:initial_difficulty(),
	    PowBlock = BP#block_plus.pow,
	    Header = block_to_header(Block),
	    Header = pow:data(PowBlock),
	    true = pow:above_min(PowBlock, Difficulty, constants:hash_size()),
	    true = Block#block.time < time_now(),
	    {BH, Block#block.prev_hash}
    end.

%check2(BP) when is_record(BP, block_plus) ->
%    check2(pow_block(BP));
check_pow(BP) ->
    Pow = BP#block_plus.pow,
    A = pow:check_pow(Pow, constants:hash_size()),
    BH = block_to_header(block(BP)), 
    B = BH == pow:data(Pow),
    A and B.
check2(BP) ->
    %check that the time is later than the median of the last 100 blocks.

    io:fwrite(packer:pack(BP)),
    io:fwrite("check2, \n"),
    %check2 assumes that the parent is in the database already.
    true = check_pow(BP),
    %PowBlock = pow_block(BP),
    %true = pow:check_pow(PowBlock, constants:hash_size()),
    Block = block(BP),
    true = Block#block.magic == constants:magic(),
    Difficulty = Block#block.difficulty,
    PH = Block#block.prev_hash,
    ParentPlus = read(PH),
    Difficulty = next_difficulty(ParentPlus),
    true = is_record(ParentPlus, block_plus),
    Prev = block(ParentPlus),
    ML = median_last(PH, constants:block_time_after_median()),
    true = Block#block.time > ML,
    Height = Block#block.height,
    MB = mine_block_ago(Height - constants:block_creation_maturity()),
    true = (Height-1) == Prev#block.height,
    {CH, AH} = {Block#block.channels, Block#block.accounts},
    {CR, AR} = absorb_txs(ParentPlus, MB, Height, Block#block.txs),
    CH = channel:root_hash(CR),
    AH = account:root_hash(AR),
    MyAddress = keys:address(),
    case MB of
	{ID, MyAddress} ->
	    keys:update_id(ID);
	    %because of hash_check, this function is only run once per block. 
	_ -> ok
    end,
    BP#block_plus{block = Block, channels = CR, accounts = AR, accumulative_difficulty = next_acc(ParentPlus, Block#block.difficulty), prev_hashes = prev_hashes(hash(Prev))}.

mine_block_ago(Height) when Height < 1 ->
    -1;
mine_block_ago(Height) ->
    BP = block:read_int(Height),
    Block = block(BP),
    %Block = pow:data(BP#block_plus.block),
    Block#block.mines_block.

median_last(BH, N) ->
    median(block_times(BH, N)).
block_times(_, 0) -> [];
block_times(<<0:96>>, N) ->
    list_many(N, 0);
block_times(H, N) ->
    BP = block:read(H),
    Block = block(BP),
    %Block = pow:data(BP#block_plus.block),
    BH2 = Block#block.prev_hash,
    T = Block#block.time,
    [T|block_times(BH2, N-1)].
list_many(0, _) -> [];
list_many(N, X) -> [X|list_many(N-1, X)].

binary_to_file(B) ->
    C = base58:binary_to_base58(B),
    H = C,
    "blocks/"++H++".db".
read(Hash) ->
    BF = binary_to_file(Hash),
    Z = db:read(BF),
    case Z of
	[] -> empty;
	A -> binary_to_term(zlib:uncompress(A))
    end.
  
lg(X) ->
    true = X > 0,
    true = is_integer(X),
    lgh(X, 0).
lgh(1, X) -> X;
lgh(N, X) -> lgh(N div 2, X+1).
read_int(N) ->%currently O(n), needs to be improved to O(lg(n))
    true = N >= 0,
    read_int(N, top:doit()).
read_int(N, BH) ->
    Block = read(BH),
    M = height(Block),
    D = M-N,
    if 
	D<0 -> io:fwrite("D is "),
	       io:fwrite(integer_to_list(D)),
	       D = 5;
	D == 0 -> Block;
	true ->
	    read_int(N, prev_hash(lg(D), Block))
    end.
	    
    
    


test() ->
    io:fwrite("top, \n"),
    block:read(top:doit()),
    PH = top:doit(),
    BP = read(PH),
    Accounts = accounts(BP),
    %Accounts = BP#block_plus.accounts,
    _ = account:get(1, Accounts),
    %{block_plus, Block, _, _, _} = make(PH, [], 1),
    Block = make(PH, [], 1),
    io:fwrite(packer:pack(Block)),
    io:fwrite("top 2, \n"),
    MBlock = mine(Block, 100000000),
    io:fwrite(packer:pack(MBlock)),
    io:fwrite("top 3, \n"),
    check2(MBlock),
    success.
new_id(N) -> 
    {Accounts, _, _, _} = tx_pool:data(),
    new_id(N, Accounts).
new_id(N, Accounts) ->
   case account:get(N, Accounts) of
       {_, empty, _} -> N;
       _ -> new_id(N+1, Accounts)
   end.
	   
mine_test() ->
    PH = top:doit(),
    %{block_plus, Block, _, _, _} = make(PH, [], keys:id()),
    BP = make(PH, [], keys:id()),
    PBlock = mine(BP, 1000000000),
    block_absorber:doit(PBlock),
    mine_blocks(10, 100000),
    success.
%mine_blocks(N) ->
%    mine_blocks(N, 1000000).
   
mine_blocks(0, _) -> success;
mine_blocks(N, Times) -> 
    PH = top:doit(),
    {_,_,_,Txs} = tx_pool:data(),
    ID = case {keys:pubkey(), keys:id()} of
	     {[], X} -> io:fwrite("you need to make an account before you can mine. look at docs/new_account.md"),
			X = 294393793232;
	     {_, -1} ->
		 NewID = new_id(1),
		 {NewID, keys:address()};
	     {_, Identity} -> Identity
	 end,
    %{block_plus, Block, _, _, _, _, _} = make(PH, Txs, ID),
    %{block_plus, Block, _, _, _, _, _} = 
    BP = make(PH, Txs, ID),
    
    %io:fwrite("mining attempt #"),
    %io:fwrite(integer_to_list(N)),
    %io:fwrite(" time "),
    %io:fwrite(integer_to_list(time_now())),
    %io:fwrite(" diff "),
    %io:fwrite(integer_to_list(Block#block.difficulty)),
    %erlang:system_info(logical_processors_available)
    Cores = guess_number_of_cpu_cores(),
    %io:fwrite(" using "),
    %io:fwrite(integer_to_list(Cores)),
    %io:fwrite(" CPU"),
    %io:fwrite("\n"),
    F = fun() ->
		case mine(BP, Times) of
		    false -> false;
		    PBlock -> 
			io:fwrite("FOUND A BLOCK !\n"),
			block_absorber:doit(PBlock)
		end
	end,
    spawn_many(Cores-1, F),
    F(),
    mine_blocks(N-1, Times).
    
spawn_many(0, _) -> ok;
spawn_many(N, F) -> 
    spawn(F),
    spawn_many(N-1, F).
guess_number_of_cpu_cores() ->    
    X = erlang:system_info(logical_processors_available),
    Y = if
        X == unknown ->
	    % Happens on Mac OS X.
            erlang:system_info(schedulers);
	is_integer(X) -> 
	    %ubuntu
	    X;
	true -> io:fwrite("number of CPU unknown, only using 1"), 1
	end,
    min(Y, free_constants:cores_to_mine()).
	
