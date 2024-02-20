package blockmon.service.webhook;

import blockmon.AppConfig;
import blockmon.entity.SubscribedTxEntity;
import blockmon.repository.SubscribedTxRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.list.MutableList;
import org.eclipse.collections.impl.list.mutable.FastList;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@RequiredArgsConstructor
public class ScheduledTransmissionManager {

    private final SubscribedTxRepository subscribedTxRepository;
    private final AppConfig appConfig;
    private final TransmissionService transmissionService;

    @Scheduled(cron = "${cron.expression.transmit.webhook}", zone = "GMT")
    @Transactional
    public void transmitWebhooks() {
        log.info("Starting scheduled service run - transmitWebhooks");
        MutableList<SubscribedTxEntity> pendingTransmissionList = FastList.newList(
                subscribedTxRepository.selectNonTransmitted(appConfig.getTransmissionRetryLimit()));

        log.info("Loaded number of tx for transmission: " + pendingTransmissionList.size());
        transmissionService.processTransmissions(pendingTransmissionList);

        log.info("Ended scheduled service run - transmitWebhooks");
    }

}
