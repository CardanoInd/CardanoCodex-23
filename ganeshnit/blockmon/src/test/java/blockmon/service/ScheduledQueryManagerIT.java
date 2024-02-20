package blockmon.service;

import blockmon.CardanohookApplicationTests;
import blockmon.constants.Constants;
import blockmon.entity.SubscribedTxEntity;
import blockmon.repository.SubscribedTxRepository;
import blockmon.repository.TransactionObjectRepository;
import blockmon.service.query.ScheduledQueryManager;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.list.MutableList;
import org.eclipse.collections.impl.list.mutable.FastList;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import static org.junit.jupiter.api.Assertions.assertTrue;

@Slf4j
public class ScheduledQueryManagerIT extends CardanohookApplicationTests {

    @Autowired
    ScheduledQueryManager queryManager;
    @Autowired
    TransactionObjectRepository transactionObjectRepository;
    @Autowired
    SubscribedTxRepository subscribedTxRepository;

    @Test
    public void whenProcessSubscribedTxs_thenSuccess() throws Exception {
        queryManager.processSubscribedTxs();
        MutableList<SubscribedTxEntity> subTxs = FastList.newList(subscribedTxRepository.findAll());

        assertTrue(subTxs.allSatisfy(e -> e.getTxStatus().getId().equals(Constants.TX_ON_CHAIN)));
    }
}
