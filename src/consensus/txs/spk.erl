-module(spk).
-export([acc1/1,acc2/1,entropy/1,
	 bets/1,space_gas/1,time_gas/1,
	 new/10,delay/1,cid/1,amount/1, 
	 nonce/1,apply_bet/4,get_paid/3,
	 run/7,settle_bet/3, slash_reward/1]).
-record(spk, {acc1, acc2, entropy, 
	      bets, space_gas, time_gas, 
	      delay, cid, amount = 0, nonce = 0,
	      slash_reward = 0}).
%scriptpubkey is the name that Satoshi gave to this part of the transactions in bitcoin.
%This is where we hold the channel contracts. They are turing complete smart contracts.
%Besides the SPK, there is the ScriptSig. Both participants of the channel sign the SPK, only one signs the SS.

acc1(X) -> X#spk.acc1.
acc2(X) -> X#spk.acc2.
bets(X) -> X#spk.bets.
delay(X) -> X#spk.delay.
entropy(X) -> X#spk.entropy.
space_gas(X) -> X#spk.space_gas.
time_gas(X) -> X#spk.time_gas.
cid(X) -> X#spk.cid.
amount(X) -> X#spk.amount.
nonce(X) -> X#spk.nonce.
slash_reward(X) -> X#spk.slash_reward.


new(Acc1, Acc2, CID, Bets, SG, TG, Delay, Nonce, Entropy, SR) ->
    %Entropy = chnnel_feeder:entropy(CID, [Acc1, Acc2])+1,
    #spk{acc1 = Acc1, acc2 = Acc2, entropy = Entropy,
	 bets = Bets, space_gas = SG, time_gas = TG,
	 delay = Delay, cid = CID, nonce = Nonce,
	 slash_reward = SR}.
    
apply_bet(Bet, SPK, Time, Space) ->
%bet is binary, the SPK portion of the script.
%SPK is the old SPK, we output the new one.
    SPK#spk{bets = [Bet|SPK#spk.bets], 
	    nonce = SPK#spk.nonce + 1, 
	    time_gas = SPK#spk.time_gas + Time, 
	    space_gas = SPK#spk.space_gas + Space}.
settle_bet(SPK, Bets, Amount) ->
    SPK#spk{bets = Bets, amount = Amount, nonce = SPK#spk.nonce + 1}.
get_paid(SPK, MyID, Amount) -> %if Amount is positive, that means money is going to Aid2.
    Aid1 = SPK#spk.acc1,
    Aid2 = SPK#spk.acc2,
    D = case MyID of
	Aid1 -> -1;
	Aid2 -> 1;
	_ -> MyID = Aid1
    end,
    SPK#spk{amount = (SPK#spk.amount + (D*Amount)), 
	    nonce = SPK#spk.nonce + 1}.
	    
run(Mode, SS, SPK, Height, Slash, Accounts, Channels) ->
    State = chalang:new_state(0, Height, Slash, 0, Accounts, Channels),
    {Amount, NewNonce, CodeShares, _, _} = run2(Mode, SS, SPK, State),
    true = NewNonce < 1000,
    Shares = shares:from_code(CodeShares),
    {Amount + SPK#spk.amount, NewNonce + (1000 * SPK#spk.nonce), Shares}.
run2(fast, SS, SPK, State) -> 
    chalang:run(SS, 
		SPK#spk.bets,
		SPK#spk.time_gas,
		SPK#spk.space_gas,
		constants:fun_limit(),
		constants:var_limit(),
		State);
run2(safe, SS, SPK, State) -> 
    %will not crash. if the thread that runs the code crashes, or takes too long, then it returns {-1,-1,-1,-1}
    S = self(),
    spawn(fun() ->
		  X = run2(fast, SS, SPK, State),
		  S ! X
	  end),
    spawn(fun() ->
		  timer:sleep(5000),%wait enough time for the chalang contracts to finish
		  S ! {-1,-1,-1,-1, -1}
	  end),
    receive 
	Z -> Z
    end.
	    
	
