-module(channel_timeout_tx).
-export([doit/4, make/5]).
-record(timeout, {aid = 0, nonce = 0, fee = 0, cid = 0}).
%If your partner is not helping you, this is how you start the process of closing the channel. 
%You should only use the final channel-state, or else your partner can punish you for cheating.
make(ID,Accounts,Channels,CID,Fee) ->
    {_, Acc, Proof} = account:get(ID, Accounts),
    {_, Channel, Proofc} = channel:get(CID, Channels),
    Acc1 = channel:acc1(Channel),
    Acc2 = channel:acc2(Channel),
    Accb = case ID of
	       Acc1 -> Acc2;
	       Acc2 -> Acc1
	   end,
    {_, _, Proof2} = account:get(Accb, Accounts),
    Nonce = account:nonce(Acc),
    Tx = #timeout{aid = ID, nonce = Nonce + 1,
		  fee = Fee, cid = CID},
    {Tx, [Proof, Proof2, Proofc]}.

doit(Tx, Channels, Accounts, NewHeight) ->
    From = Tx#timeout.aid,
    CID = Tx#timeout.cid,
    {_, Channel, _} = channel:get(CID, Channels),
    CA = channel:amount(Channel),
    false = CA == 0,
    LM = channel:last_modified(Channel),
    TD = NewHeight - LM,
    true = TD >= channel:delay(Channel),
    %Mode = channel:mode(Channel),
    Aid1 = channel:acc1(Channel),
    Aid2 = channel:acc2(Channel),
    Amount = channel:amount(Channel),
    Fee = Tx#timeout.fee,
    SR = channel:slash_reward(Channel),
    Acc1 = account:update(Aid1, Accounts, channel:bal1(Channel)-Amount-SR, none, NewHeight),
    Acc2 = account:update(Aid2, Accounts, channel:bal2(Channel)+Amount-SR, none, NewHeight),
    Accounts2 = account:write(Accounts, Acc1),
    Accounts3 = account:write(Accounts2, Acc2),
    Slasher = channel:slasher(Channel),
    Accounts4 = 
	case Slasher of
	    0 -> Accounts3;
	    X ->
		Acc3 = account:update(X, Accounts3, SR*2, none, NewHeight), 
		accounts:write(Accounts3, Acc3)
	end,
    Acc4 = account:update(From, Accounts4, -Fee, none, NewHeight),
    NewAccounts = account:write(Accounts4, Acc4),
    NewChannels = channel:delete(CID, Channels),
    {NewChannels, NewAccounts}.

