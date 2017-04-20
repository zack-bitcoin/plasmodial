-module(oracle_new_tx).
-export([test/0, doit/3, make/6]).
-record(oracle_new, {from = 0, nonce = 0, fee = 0, question = <<>>, start, id}).
%This asks the oracle a question.
%The oracle can only answer true/false questions.
%Running the oracle costs a fee which is used as a reward to get people to use the oracle.
%The fact that an oracle exists is recorded on the blockchain in a way that is accessible to the VM. So we can use channels to make smart contracts to raise funds to run the oracle.
%The entire text of the question is written into the transaction, but only the hash of the text is stored into a consensus state merkel tree.
%The oracle has a start-date written in it. Trading doesn't start until the start-date.
%The oracle can be published before we know the outcome of the question, that way the oracle id can be used to make channel contracts that bet on the eventual outcome of the oracle.
make(From, Fee, Question, Start, ID, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _Proof} = account:get(From, Accounts),
    Tx = #oracle_new{from = From, nonce = account:nonce(Acc) + 1, fee = Fee, question = Question, start = Start, id = ID},
    {Tx, []}.
doit(Tx, Trees, NewHeight) ->
    Accounts = trees:accounts(Trees),
    From = Tx#oracle_new.from,
    Facc = account:update(From, Accounts, -Tx#oracle_new.fee-constants:oracle_initial_liquidity(), Tx#oracle_new.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),
    Oracles = trees:oracles(Trees),
    Starts = Tx#oracle_new.start,
    true = (Starts - NewHeight) < constants:oracle_future_limit(),
    ID = Tx#oracle_new.id,
    Question = Tx#oracle_new.question,
    true = is_binary(Question),
    QH = testnet_hasher:doit(Question),
    Diff = constants:even_diff(),
    ON = oracles:new(ID, QH, Starts, From, Diff),
    {_, empty, _} = oracles:get(ID, Oracles),
    NewOracles = oracles:write(ON, Oracles),
    Trees2 = trees:update_oracles(Trees, NewOracles),
    trees:update_accounts(Trees2, NewAccounts).
    
test() ->
    success.
