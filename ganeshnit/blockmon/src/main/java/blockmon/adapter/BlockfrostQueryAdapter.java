package blockmon.adapter;

import blockmon.model.blockfrost.Transaction;

public interface BlockfrostQueryAdapter {

    public Transaction checkTxStatus(String hash);

}