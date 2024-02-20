package blockmon.adapter;

import rest.koios.client.backend.api.transactions.model.TxInfo;

import java.util.List;

public interface KoiosQueryAdapter {

    public List<TxInfo> checkTxStatus(List<String> hashes);

}