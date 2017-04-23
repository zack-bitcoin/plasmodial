-module(oracle_new_tx).
-export([test/0, doit/3, make/10]).
-record(oracle_new, {from = 0, 
		     nonce = 0, 
		     fee = 0, 
		     question = <<>>, 
		     start, 
		     id, 
		     recent_price, %if this is a governance oracle, or if it is asking a question, then we need to reference another oracle that closed recently with the state "bad". We reference it so we know the current price of shares.
		     difficulty, 
		     governance, 
		     governance_amount}).
%This asks the oracle a question.
%The oracle can only answer true/false questions.
%Running the oracle costs a fee which is used as a reward to get people to use the oracle.
%The fact that an oracle exists is recorded on the blockchain in a way that is accessible to the VM. So we can use channels to make smart contracts to raise funds to run the oracle.
%The entire text of the question is written into the transaction, but only the hash of the text is stored into a consensus state merkel tree.
%The oracle has a start-date written in it. Trading doesn't start until the start-date.
%The oracle can be published before we know the outcome of the question, that way the oracle id can be used to make channel contracts that bet on the eventual outcome of the oracle.
make(From, Fee, Question, Start, ID, Difficulty, Recent, Governance, GovAmount, Trees) ->
    Accounts = trees:accounts(Trees),
    {_, Acc, _Proof} = account:get(From, Accounts),
    Tx = #oracle_new{from = From, nonce = account:nonce(Acc) + 1, fee = Fee, question = Question, start = Start, id = ID, recent_price = Recent, difficulty = Difficulty, governance = Governance, governance_amount = GovAmount},
    {Tx, []}.
doit(Tx, Trees0, NewHeight) ->
    %If the question is <<"">>, let it run.
    %If the question is not <<"">>, then they need to show that a different oracle with the question "" recently returned "bad", and the difficulty of this oracle is 1/2 as high as that oracle.
    Oracles = trees:oracles(Trees0),
    Gov = Tx#oracle_new.governance,
    GovAmount = Tx#oracle_new.governance_amount,
    true = GovAmount > -1,
    true = GovAmount < constants:governance_change_limit(),
    Question = Tx#oracle_new.question,
    {_, Recent, _} = oracles:get(Tx#oracle_new.recent_price,Oracles),
    io:fwrite("recent oracle "),
    io:fwrite(packer:pack(Recent)),
    io:fwrite("\n"),
    Trees = 
	case Gov of
	    0 -> Trees0;
	    G ->
		true = GovAmount > 0,
		3 = oracles:result(Recent),
		true = NewHeight - oracles:done_timer(Recent) < constants:governance_delay(),
		Dif = oracles:difficulty(Recent),
		Dif = Tx#oracle_new.difficulty,
		Question = <<"">>,
		GovTree = trees:governance(Trees0),
		{_, GVar, _} = governance:get(G, GovTree),
		false = governance:is_locked(GVar),
		NewGVar = governance:lock(GVar),
		NewGovTree = governance:write(NewGVar, GovTree),
		trees:update_governance(Trees0, NewGovTree)
	end,
    ok = case Question of
	     <<"">>-> ok;
	     _Q -> 
		 %get the recent oracle, make sure it's question was <<"">>, make sure our difficulty is half as high as that difficulty.
		 %true = size(Q) < constants:maximum_question_size(),
		 0 = GovAmount,
		 Di = oracles:difficulty(Recent) div 2,
		 Di = Tx#oracle_new.difficulty,
		 3 = oracles:result(Recent),
		 true = NewHeight - oracles:done_timer(Recent) < constants:question_delay(),
		 ok
	 end,
    Accounts = trees:accounts(Trees),
    From = Tx#oracle_new.from,
    Facc = account:update(From, Accounts, -Tx#oracle_new.fee-constants:oracle_initial_liquidity(), Tx#oracle_new.nonce, NewHeight),
    NewAccounts = account:write(Accounts, Facc),
    Starts = Tx#oracle_new.start,
    true = (Starts - NewHeight) < constants:oracle_future_limit(),
    ID = Tx#oracle_new.id,
    Question = Tx#oracle_new.question,
    true = is_binary(Question),
    QH = testnet_hasher:doit(Question),
    Diff = Tx#oracle_new.difficulty,
    ON = oracles:new(ID, QH, Starts, From, Diff, Gov, GovAmount),
    {_, empty, _} = oracles:get(ID, Oracles),
    NewOracles = oracles:write(ON, Oracles),
    Trees2 = trees:update_oracles(Trees, NewOracles),
    trees:update_accounts(Trees2, NewAccounts).
    
test() ->
    success.
