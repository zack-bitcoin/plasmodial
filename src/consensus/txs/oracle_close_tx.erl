-module(oracle_close_tx).
-export([test/0]).
%If there is a lot of open orders for one type of share in an oracle for a long enough period of time, then this transaction can be done.
%This ends betting in the market.
%The fee that was used to start the oracle is the final bet included. It bets against the winning outcome.
doit(Tx, Trees, NewHeight) ->
    %change the oracle's type from 0 to whatever the outcome should be.
    ok.
test() ->
    success.
