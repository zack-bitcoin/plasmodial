-module(oracle_unmatched_tx).
-export([test/0, make/5, doit/3]).
%If you had money in orders in the oracle order book when the oracle_close transaction happened, this is how you get the money out.
-record(unmatched, {from, nonce, fee, oracle_id, order_id}).

make(From, Fee, OracleID, OrderID, Accounts) ->
    {_, Acc, Proof} = account:get(From, Accounts),
    Tx = #unmatched{from = From, nonce = account:nonce(Acc) + 1, fee = Fee, oracle_id = OracleID, order_id = OrderID},
    {Tx, [Proof]}.

doit(Tx, Trees, NewHeight) ->
    OracleID = Tx#unmatched.oracle_id,
    OrderID = Tx#unmatched.order_id,
    AID = Tx#unmatched.from,
    Oracles = trees:oracles(Trees),
    {_, Oracle, _} = oracles:get(OracleID, Oracles),
    Orders = oracles:orders(Oracle),
    {_, Order, _} = orders:get(OrderID, Orders),
    Amount = orders:amount(Order),
    Orders2 = orders:remove(OrderID, Orders),
    Oracle2 = oracles:set_orders(Oracle, Orders2),
    Oracles2 = oracles:write(Oracle2, Oracles),
    Trees2 = trees:update_oracles(Trees, Oracles2),

    Accounts = trees:accounts(Trees),
    Facc = account:update(AID, Accounts, Amount-Tx#unmatched.fee, Tx#unmatched.nonce, NewHeight),
    Accounts2 = account:write(Accounts, Facc),
    trees:update_accounts(Trees2, Accounts2).

test() ->
    success.
