package blockmon.service.webhook;

import blockmon.entity.SubscribedTxEntity;
import org.eclipse.collections.api.list.MutableList;

public interface TransmissionService {

    void processTransmissions(MutableList<SubscribedTxEntity> pendingTransmissionList);


}
