package blockmon.service.query;

import blockmon.adapter.koios.KoiosAdapterMainnet;
import blockmon.constants.Constants;
import blockmon.repository.StatusRepository;
import blockmon.service.SubscriptionService;
import com.fasterxml.jackson.databind.ObjectMapper;
import blockmon.entity.SubscribedTxEntity;
import blockmon.entity.TransactionObjEntity;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.RichIterable;
import org.eclipse.collections.impl.utility.Iterate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;
import rest.koios.client.backend.api.transactions.model.TxInfo;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;

@Service("koiosQueryService")
@AllArgsConstructor
@Slf4j
public class KoiosQueryService implements QueryService {

    private final KoiosAdapterMainnet koiosAdapterMainnet;
    private final SubscriptionService subscriptionService;
    private final StatusRepository statusRepository;
    private final ObjectMapper objectMapper;

    @Override
    @Async(value = "koiosExecutor")
    public Future<Boolean> queryAndUpdateSubscribedTx(RichIterable<SubscribedTxEntity> subTxs) {
        try {
            Thread.sleep((long) (Math.random() * 2000));
        } catch (InterruptedException e) {
            log.error("Could not sleep koiosExecutor thread");
            throw new RuntimeException(e);
        }

        List<TransactionObjEntity> txObjs = new ArrayList<>();
        try {
            List<String> txHashes = Iterate.collect(subTxs, e -> e.getTxHash()).stream().toList();
            List<TxInfo> txInfos = koiosAdapterMainnet.checkTxStatus(txHashes);

            for (TxInfo txInfo : txInfos) {
                String statusId = txInfo.getBlockHash() != null ? Constants.TX_ON_CHAIN : Constants.TX_PENDING;
                SubscribedTxEntity subTx = subTxs.detect(e -> e.getTxHash().equals(txInfo.getTxHash()));
                subTx.setTxStatus(statusRepository.getReferenceById(statusId));
                subscriptionService.safelyAddAsTxObj(subTx, txInfo, txObjs);
            }
        } catch (Exception ex) {
            subTxs.forEach(e -> e.setTxStatus(statusRepository.getReferenceById(Constants.TX_PROCESS_ERROR)));
        } finally {
            subscriptionService.saveSubTxsWith(subTxs, txObjs);
        }
        return new AsyncResult<>(true);
    }


}
