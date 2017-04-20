-module(oracle_bet_tx).
-export([test/0]).
-record(oracle_bet, {id, type, many}).
%This is how you can participate in an existing oracle.
%The market is an order book with 3 types of shares: "true", "false", "bad_question"
%All trades are matched into the order book in pairs at even odds.
%So the order book only stores 1 kind of order at a time.
%If you want your order to be held in the order book, it needs to be bigger than a minimum size.
%There is a maximum number of orders that can be stored in the order book at a time.
%If your order isn't big enough to be in the order book, you cannot buy shares of the type that are stored in the order book.
doit(Tx, Trees, NewHeight) ->
    Oracles = trees:oracles(Trees),
    Oracle = oracles:get(Tx#oracle_bet.id, Oracles),
    true = NewHeight > oracles:starts(Oracle),
    %take some money from them. 
    %if an order is matched, then give bets to the two accounts who were matched.
    ok.
test() ->
    success.
