-module(existence_tx).
-export([doit/5, make/4]).
-record(ex, {from, fee = 0, nonce = 0, commit = 0}).

make(From, Fee, Data, AccountRoot) ->
    {_, Acc, Proof} = account:get(From, AccountRoot),
    Nonce = account:nonce(Acc) + 1,
    Tx = #ex{from = From, fee = Fee, nonce = Nonce, commit = testnet_hasher:doit(Data)},
    {Tx, [Proof]}.
doit(Tx, Channels, Accounts, Commits, NewHeight) ->
    From = Tx#ex.from,
    C = Tx#ex.commit,
    {_, empty, _} = existence:get(C, Commits),
    NewCommits = existence:write(From, C, Commits),
    Acc = account:update(From, Accounts, -Tx#ex.fee, Tx#ex.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Acc),
    {Channels, NewAccounts, NewCommits}.
    

    
