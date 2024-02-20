package blockmon.adapter.koios;

import blockmon.adapter.KoiosQueryAdapter;
import blockmon.exception.BlockfrostAdaptorException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import rest.koios.client.backend.api.base.Result;
import rest.koios.client.backend.api.base.exception.ApiException;
import rest.koios.client.backend.api.transactions.TransactionsService;
import rest.koios.client.backend.api.transactions.model.TxInfo;

import java.util.List;

@Service
@Slf4j
public class KoiosAdapterMainnet implements KoiosQueryAdapter {

    private TransactionsService transactionsService;

    public KoiosAdapterMainnet(@Qualifier("koiosMainnetClient") TransactionsService koiosClient) {
        this.transactionsService = koiosClient;
    }

    @Override
    @Transactional(noRollbackFor = {BlockfrostAdaptorException.class})
    public List<TxInfo> checkTxStatus(List<String> hashes) {

        if (hashes == null || hashes.isEmpty()) {
            return null;
        }

        log.info("Issuing koios query for hashes: " + hashes.toString());
        List<TxInfo> txResponse = null;
        try {
            Result<List<TxInfo>> transactionInformationResult = transactionsService
                    .getTransactionInformation(hashes, null);
            if (!transactionInformationResult.isSuccessful()) {
                log.error("Koios transactionInformationResult unsuccessful - ");
                return null;
            }
            txResponse = transactionInformationResult.getValue();
        } catch (RuntimeException e) {
            log.error("RuntimeException while querying koios - ", e);
            throw e;
        } catch (ApiException e) {
            log.error("ApiException from koios libs - ", e);
            throw new RuntimeException(e);
        }
        return txResponse;
    }

}