package blockmon.service;

import blockmon.CardanohookApplicationTests;
import blockmon.entity.SubscribedTxEntity;
import blockmon.entity.TransactionObjEntity;
import blockmon.model.enums.Status;
import blockmon.repository.SubscribedTxRepository;
import blockmon.repository.TransactionObjectRepository;
import blockmon.service.query.QueryService;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.impl.list.mutable.FastList;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.util.concurrent.Future;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@Slf4j
public class KoiosQueryServiceIT extends CardanohookApplicationTests {

    @Autowired @Qualifier("koiosQueryService")
    QueryService koiosQueryService;
    @Autowired
    TransactionObjectRepository transactionObjectRepository;
    @Autowired
    SubscribedTxRepository subscribedTxRepository;

    @Test
    public void whenQueryAndUpdateSubscribedTxMainnet_thenSuccess() throws Exception {
        String txHash = testMainnetHash2;
        if (transactionObjectRepository.existsByTxHash(txHash)) {
            transactionObjectRepository.deleteByTxHash(txHash);
        }
        SubscribedTxEntity subTx = subscribedTxRepository.findByTxHash(txHash).orElseThrow();
        Future<Boolean> future = koiosQueryService.queryAndUpdateSubscribedTx(FastList.newListWith(subTx));

        while (true) {
            if (future.isDone()) {
                log.info("queryAndUpdate completed");
                break;
            }
            log.error("Continue doing something else. ");
            Thread.sleep(1000);
        }

        TransactionObjEntity txE = transactionObjectRepository.findByTxHash(txHash).orElseThrow();
        assertNotNull(txE);
        subTx = subscribedTxRepository.findByTxHash(txHash).orElseThrow();
        assertEquals(Status.TX_ON_CHAIN.value, subTx.getTxStatus().getId());
    }
}
