-module(tree_test).
-export([test/0]).

test() ->
    S = success,
    S = account:test(),
    S = channel:test(),
    S = existence:test(),
    S = oracles:test(),
    S = burn:test(),
    S = oracle_bets:test(),
    S = shares:test(),
    S = orders:test(),
    S = governance:test(),
    S.
    
