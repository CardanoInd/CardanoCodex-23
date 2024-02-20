package blockmon.service;


import blockmon.AppConfig;
import blockmon.dto.SubscribedTxDto;
import blockmon.exception.InvalidPayloadException;
import blockmon.repository.QueryTargetRepository;
import blockmon.repository.StatusRepository;
import blockmon.repository.SubscribedTxRepository;
import blockmon.repository.TransactionObjectRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import blockmon.entity.SubscribedTxEntity;
import blockmon.entity.TransactionObjEntity;
import blockmon.model.SubscriptionRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.map.MutableMap;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

import static blockmon.constants.Constants.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class SubscriptionServiceImpl implements SubscriptionService {
    private final SubscribedTxRepository repository;
    private final TransactionObjectRepository txTransactionObjectRepository;
    private final StatusRepository statusRepository;
    private final QueryTargetRepository queryTargetRepository;
    private final ObjectMapper objectMapper;
    private final AppConfig appConfig;


    @Transactional
    @Override
    public SubscribedTxDto registerTx(SubscriptionRequest req) {
        if (!appConfig.allowedNetworkNames().contains(req.getNetwork())) {
            throw new InvalidPayloadException("Invalid network parameter in request body");
        }
        String nw = req.getNetwork().equalsIgnoreCase("mainnet") ? MAINNET : PREPROD;
        SubscribedTxEntity stx = SubscribedTxEntity.builder()
                .txHash(req.getTxHash()).refId(UUID.randomUUID().toString())
                .network(nw)
                .webhookUrl(req.getWebhookUrl())
                .txStatus(statusRepository.getReferenceById(NOT_CHECKED))
                .queryTarget(queryTargetRepository.getReferenceById(AppConfig.DEFAULT_QT)).build();
        repository.save(stx);
        return toStxDto(stx);
    }

    private SubscribedTxDto toStxDto(SubscribedTxEntity stx) {
        return SubscribedTxDto.builder()
                .refId(stx.getRefId()).txHash(stx.getTxHash())
                .build();
    }

    @Override
    public UUID deRegisterTx(String apiKey, String hash) {
        return null;
    }

    @Override
    public MutableMap<String, Object> checkTxStatus(String apiKey, String hash) {
        return null;
    }

    @Override
    @Transactional
    public void saveSubTxsWith(Iterable<SubscribedTxEntity> subTxEs, List<TransactionObjEntity> txObjs) {
        repository.saveAll(subTxEs);
        txTransactionObjectRepository.saveAll(txObjs);
    }

    @Override
    public void safelyAddAsTxObj(SubscribedTxEntity subTx, Object txInfo, List<TransactionObjEntity> txObjs) {
        if (txInfo == null) {
            return;
        }
        try {
            TransactionObjEntity e = TransactionObjEntity.builder()
                    .refId(subTx.getRefId()).txHash(subTx.getTxHash()).network(subTx.getNetwork())
                    .queryTarget(subTx.getQueryTarget()).txResponse(objectMapper.writeValueAsString(txInfo))
                    .build();
            txObjs.add(e);
        } catch (JsonProcessingException ex) {
            log.error("Safely ignored json processing error", ex);
        }

    }

}
