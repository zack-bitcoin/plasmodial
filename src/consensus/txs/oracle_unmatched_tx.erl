-module(oracle_unmatched_tx).
-export([test/0]).
%If you had money in orders in the oracle order book when the oracle_close transaction happened, this is how you get the money out.

doit(Tx, Trees, NewHeight) ->
    %give them their money.
    %delete their order from the order book.
    ok.

test() ->
    success.
