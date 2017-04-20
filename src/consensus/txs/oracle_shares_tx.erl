-module(oracle_shares_tx).
-export([test/0, doit/3]).

%If you bet in an oracle, and the oracle has closed, this is how you get your shares out.
%If you bet on the winning outcome, then you get positive shares. If you bet on one of the losing outcomes, then you get negative shares.
%[you can read about shares here](docs/shares.md)
%The difficulty of the shares was announced when the oracle was launched.
doit(Tx, Trees, NewHeight) ->
    %transform their bets into shares.
    ok.
test() ->
    success.
