package blockmon.service.query;

import blockmon.entity.SubscribedTxEntity;
import org.eclipse.collections.api.RichIterable;

import java.util.concurrent.Future;

public interface QueryService {

    Future<Boolean> queryAndUpdateSubscribedTx(RichIterable<SubscribedTxEntity> subTxEs);

}
