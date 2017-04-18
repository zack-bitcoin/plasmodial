check that the slasher is being rewarded in channel_timeout_tx

implement the oracle transaction types.
upgrade spending so it can spend shares.
upgrade delete_account so it can send shares.

javascript light wallets need to be able to do all the channel stuff that full nodes do.

Channels should only hold AE tokens, but when a channel settles, it should be able to split the AE tokens into equal pairs of any of the types of tokens, and give one side of the pair to one account and the other side to the other account. This way we can use channels to trade all types of tokens.


in the white paper we should explain the centralized and trustless exchanges.


blog post about channels that use untrusted third parties to be secure. So you don't have to stay online all the time.


in channel_solo_close we update the channel:amount written on-chain. this is a problem, because when we slash, we want to undo the channel_solo_close.
We need to look at the tests, maybe we are testing the wrong thing, or we aren't testing the right thing.

the big case statement at line 48 of channel_solo_close seems bad. shouldn't we make sure that both balances are positive, not just one?

It is currently possible for an attacker to trick us into ignoring a good block. They trick us into storing a good blocks hash into block_hashes. They give us a good block's header, but mix in some bad transactions, or censor a transaction, or they don't give us some of the merkel tree we need to verify the transactions.
To fix this, each header should contain a hash of all the transactions. We should include the signatures in this hash. This gives us a guarantee that all the data is available at block:check1.

We need a way to close a channel when your partner refuses to sign any SPK. It can either be a new tx type, or we can make solo_close more complex.


constants:difficulty_bits() might be too big.


the new way of doing channel slash has a different problem.
We need the channel to finish at the highest nonce possible. The third party could be bribed to choose a different final state.
We need some way to do the channel_slash transaction again and again, until a higher nonce cannot be found.
Each slasher puts up a deposit, and takes the deposit of the previous.
If the same channel_slash exists for a long enough time period, then anyone can do a channel_timeout transaction, to close the channel.
We need to also add a way for the two parties to work together to close the channel early, so they don't have to wait to do a timeout_tx. We can either make a new tx, or make channel_team_close more complicated.


We should use a CLI program to talk to the node instead of using erlang directly.
It should be able to access everything in /src/networking/internal_handler.erl
We should add everything from easy to internal_handler.erl

We need to update download_blocks so that peers get ranked, and we spend more time talking to higher-ranked peers.

There is a problem where if you crash while syncing with a peer, then you skip trying to sync with any peer lower on the list. this is very bad.

make the api networking/handler be entirely encrypted. This is to protect information about the channels. https://github.com/BumblebeeBat/pink_crypto/blob/master/src/encryption.erl

download_blocks could be more efficient.

maybe nodes need to advertise their own IP/port combo as a peer?

It would be nice if there were some macros for chalang/src/compiler_lisp2.erl that did backtracking. that way we wouldn't have to think about control flow when making smart contracts.




Updates for next time we restart at a genesis block:


each tx with a fee needs a to reference a recent hash. Everyone needs to be incentivized to make the hash as recent as possible.


We need to reward the miner with the transaction fees, to incentivize him to include them. block:absorb_txs

Make sure that if something was garbage collected from a merkel tree, and we try accessing the thing, it gives a different message than trying to access something that doesn't exist.