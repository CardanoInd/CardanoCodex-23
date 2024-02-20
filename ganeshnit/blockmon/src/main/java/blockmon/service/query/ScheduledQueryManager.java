package blockmon.service.query;

import blockmon.AppConfig;
import blockmon.entity.SubscribedTxEntity;
import blockmon.repository.QueryTargetRepository;
import blockmon.repository.SubscribedTxRepository;
import blockmon.constants.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.RichIterable;
import org.eclipse.collections.api.list.MutableList;
import org.eclipse.collections.api.map.MutableMap;
import org.eclipse.collections.api.multimap.list.MutableListMultimap;
import org.eclipse.collections.api.partition.list.PartitionMutableList;
import org.eclipse.collections.impl.list.mutable.FastList;
import org.eclipse.collections.impl.map.mutable.UnifiedMap;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

@Service
@Slf4j
@RequiredArgsConstructor
public class ScheduledQueryManager {

    private final SubscribedTxRepository subscribedTxRepository;
    private final QueryService blockfrostQueryService;
    private final QueryService koiosQueryService;
    private final QueryTargetRepository queryTargetRepository;

    @Scheduled(cron = "${cron.expression.query.tx.status}", zone = "GMT")
    @Transactional
    public void processSubscribedTxs() {
        log.info("Starting scheduled service run - processSubscribedTxs");
        MutableList<SubscribedTxEntity> pendingList = FastList.newList(
                subscribedTxRepository.selectNonConfirmedTxs(OffsetDateTime.now().minusSeconds(AppConfig.SECONDS_WAIT_BEFORE_QUERY)));
        log.info("Loaded number of tx for querying: " + pendingList.size());
        MutableMap<String, MutableList<SubscribedTxEntity>> queryGroups = getQueryGroups(pendingList);

        Future<Boolean> future;
        MutableList<SubscribedTxEntity> koiosGrp = queryGroups.get(Constants.QUERY_TARGET_KOIOS);
        log.info("Number of tx for koios: " + koiosGrp.size());
        if (koiosGrp.size() > 0) {
            for (RichIterable<SubscribedTxEntity> chunk : koiosGrp.chunk(10)) {
                future = koiosQueryService.queryAndUpdateSubscribedTx(chunk);
                try {
                    future.get(AppConfig.QUERY_THREAD_TIMEOUT_MINUTES, TimeUnit.MINUTES);
                } catch (TimeoutException tex) {
                    log.error(tex.getMessage());
                    future.cancel(true);
                } catch (ExecutionException | InterruptedException ex) {
                    log.error(ex.getMessage());
                }
            }
        }


        MutableList<SubscribedTxEntity> blockfrostGrp = queryGroups.get(Constants.QUERY_TARGET_BLOCKFROST);
        log.info("Number of tx for blockfrost: " + blockfrostGrp.size());
        if (blockfrostGrp.size() > 0) {
            for (RichIterable<SubscribedTxEntity> chunk : blockfrostGrp.chunk((int) Math.ceil((float) blockfrostGrp.size() / 3))) {
                future = blockfrostQueryService.queryAndUpdateSubscribedTx(chunk);
                try {
                    future.get(AppConfig.QUERY_THREAD_TIMEOUT_MINUTES, TimeUnit.MINUTES);
                } catch (TimeoutException tex) {
                    log.error(tex.getMessage());
                    future.cancel(true);
                } catch (ExecutionException | InterruptedException ex) {
                    log.error(ex.getMessage());
                }
            }
        }

        log.info("Ended scheduled service run - processSubscribedTxs");

    }

    public UnifiedMap<String, MutableList<SubscribedTxEntity>> getQueryGroups(MutableList<SubscribedTxEntity> pendingList) {
        MutableListMultimap<String, SubscribedTxEntity> groupedSubTxs = pendingList.groupBy(e -> e.getQueryTarget().getId());
        MutableList<SubscribedTxEntity> koiosGroup = groupedSubTxs.get(Constants.QUERY_TARGET_KOIOS);
        PartitionMutableList<SubscribedTxEntity> koiosPartitionedGroup = koiosGroup.partition(e -> e.getNetwork().equalsIgnoreCase(Constants.MAINNET));
        MutableList<SubscribedTxEntity> blockfrostGroup = FastList.newList(groupedSubTxs.get(Constants.QUERY_TARGET_BLOCKFROST));
        MutableList<SubscribedTxEntity> additionalForBf = koiosPartitionedGroup.getRejected();
        additionalForBf.forEach(e -> e.setQueryTarget(queryTargetRepository.getReferenceById(Constants.QUERY_TARGET_BLOCKFROST)));
        blockfrostGroup.addAll(additionalForBf);
        return UnifiedMap.newWithKeysValues(
                Constants.QUERY_TARGET_KOIOS, koiosPartitionedGroup.getSelected(),
                Constants.QUERY_TARGET_BLOCKFROST, blockfrostGroup);
    }

}
