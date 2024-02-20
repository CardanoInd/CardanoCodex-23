package blockmon.service;


import blockmon.dto.SubscribedTxDto;
import blockmon.entity.SubscribedTxEntity;
import blockmon.entity.TransactionObjEntity;
import blockmon.model.SubscriptionRequest;
import org.eclipse.collections.api.map.MutableMap;

import java.util.List;
import java.util.UUID;

public interface SubscriptionService {

    SubscribedTxDto registerTx(SubscriptionRequest subscriptionRequest);

    UUID deRegisterTx(String apiKey, String hash);

    MutableMap<String, Object> checkTxStatus(String apiKey, String hash);

    void saveSubTxsWith(Iterable<SubscribedTxEntity> subTxEs, List<TransactionObjEntity> txObjStrings);

    void safelyAddAsTxObj(SubscribedTxEntity subTx, Object txInfo, List<TransactionObjEntity> txObjs);
}