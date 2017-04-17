-module(spend_tx).
-export([doit/3, make/5]).
-record(spend, {from = 0, nonce = 0, fee = 0, to = 0, amount = 0}).
make(To, Amount, Fee, Id, Accounts) ->
    {_, Acc, Proof} = account:get(Id, Accounts),
    {_, _Acc2, Proof2} = account:get(To, Accounts),
    Tx = #spend{from = Id, nonce = account:nonce(Acc) + 1, to = To, amount = Amount, fee = Fee},
    {Tx, [Proof, Proof2]}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    From = Tx#spend.from,
    To = Tx#spend.to,
    false = From == To,
    A = Tx#spend.amount,
    Facc = account:update(From, Accounts, -A-Tx#spend.fee, Tx#spend.nonce, NewHeight),
    Tacc = account:update(To, Accounts, A, none, NewHeight),
    Accounts2 = account:write(Accounts, Facc),
    NewAccounts = account:write(Accounts2, Tacc),
    trees:update_accounts(Trees, NewAccounts).
