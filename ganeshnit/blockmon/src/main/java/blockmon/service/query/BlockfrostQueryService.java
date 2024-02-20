package blockmon.service.query;

import blockmon.adapter.BlockfrostQueryAdapter;
import blockmon.adapter.blockfrost.BlockfrostAdapterMainnet;
import blockmon.adapter.blockfrost.BlockfrostAdapterPreprod;
import blockmon.constants.Constants;
import blockmon.repository.StatusRepository;
import blockmon.service.SubscriptionService;
import com.fasterxml.jackson.databind.ObjectMapper;
import blockmon.entity.SubscribedTxEntity;
import blockmon.entity.TransactionObjEntity;
import blockmon.model.blockfrost.Transaction;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.RichIterable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;

@Service("blockfrostQueryService")
@AllArgsConstructor
@Slf4j
public class BlockfrostQueryService implements QueryService {

    private final BlockfrostAdapterMainnet mainnetAdapter;
    private final BlockfrostAdapterPreprod preprodAdapter;
    private final SubscriptionService subscriptionService;
    private final StatusRepository statusRepository;
    private final ObjectMapper objectMapper;


    @Override
    @Async(value = "blockfrostExecutor")
    public Future<Boolean> queryAndUpdateSubscribedTx(RichIterable<SubscribedTxEntity> subTxs) {
        List<TransactionObjEntity> txObjs = new ArrayList<>();
        try {
            for (SubscribedTxEntity subTx : subTxs) {
                try {
                    Thread.sleep((long) (Math.random() * 2000));
                } catch (InterruptedException e) {
                    log.error("Could not sleep blockfrostExecutor thread");
                    throw new RuntimeException(e);
                }
                BlockfrostQueryAdapter bfAdapter = subTx.getNetwork().equalsIgnoreCase("mainnet") ? mainnetAdapter : preprodAdapter;

                String statusId = null;
                Transaction tx = null;
                try {
                    tx = bfAdapter.checkTxStatus(subTx.getTxHash());
                    statusId = (tx != null && tx.getBlock() != null) ? Constants.TX_ON_CHAIN : Constants.TX_PENDING;
                } catch (Exception e) {
                    statusId = Constants.TX_PROCESS_ERROR;
                } finally {
                    subTx.setTxStatus(statusRepository.getReferenceById(statusId));
                    subscriptionService.safelyAddAsTxObj(subTx, tx, txObjs);
                }
            }
        } finally {
            subscriptionService.saveSubTxsWith(subTxs, txObjs);
        }
        return new AsyncResult<>(true);
    }
}
