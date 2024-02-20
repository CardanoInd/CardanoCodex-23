package blockmon.service;


import blockmon.constants.Constants;
import blockmon.dto.webhook.WebhookResponseDto;
import blockmon.repository.MockWebhookReceiptRepository;
import blockmon.repository.QueryTargetRepository;
import blockmon.repository.StatusRepository;
import blockmon.repository.TransactionObjectRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import blockmon.entity.MockWebhookReceiptEntity;
import blockmon.entity.TransactionObjEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class MockWebhookReceiptServiceImpl implements MockWebhookReceiptService {
    private final MockWebhookReceiptRepository repository;
    private final TransactionObjectRepository transactionObjectRepository;
    private final StatusRepository statusRepository;
    private final QueryTargetRepository queryTargetRepository;
    private final ObjectMapper objectMapper;

    @Override
    @Transactional
    public boolean persistPayload(WebhookResponseDto payload) {
        MockWebhookReceiptEntity mockWebhookReceiptEntity = toEntity(payload);
        repository.save(mockWebhookReceiptEntity);
        try {
            TransactionObjEntity txObj = new TransactionObjEntity();
            BeanUtils.copyProperties(payload, txObj);
            txObj.setTxResponse(objectMapper.writeValueAsString(payload.getFullTxDetailsWithOptionalFields()));
            txObj.setQueryTarget(queryTargetRepository.getReferenceById(Constants.QUERY_TARGET_KOIOS));
            transactionObjectRepository.save(txObj);
        } catch (JsonProcessingException e) {
            log.error("Json parse error ignored", e);
        }
        return true;
    }

    private MockWebhookReceiptEntity toEntity(WebhookResponseDto payload) {
        MockWebhookReceiptEntity e = new MockWebhookReceiptEntity();
        BeanUtils.copyProperties(payload, e);
        e.setTxStatus(statusRepository.getReferenceById(payload.getTxStatus().getId()));
        return e;
    }

}
