-module(governance).
-export([]).


name2number(block_reward) -> 1;
name2number(gas_limit) -> 2;
name2number(max_block_size) -> 3;
name2number(create_channel_fee) -> 4;
name2number(delete_channel_reward) -> 5;
name2number(create_account_fee) -> 6;
name2number(delete_account_reward) -> 7;
name2number(account_fee) -> 8;
name2number(channel_rent) -> 9;
name2number(account_rent) -> 10;
name2number(block_time) -> 11;
name2number(oracle_future_limit) -> 12;
name2number(shares_conversion) -> 13;
name2number(fun_limit) -> 14;
name2number(var_limit) -> 15;
name2number(comment_limit) -> 16;
name2number(block_creation_maturity) -> 17;
name2number(oracle_initial_liquidity) -> 18;
name2number(minimum_oracle_time) -> 19;
name2number(maximum_question_sizze) -> 20.
