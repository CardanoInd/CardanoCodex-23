package blockmon.service.webhook;

import blockmon.adapter.TransmissionAdapter;
import blockmon.dto.webhook.TxStatusDto;
import blockmon.dto.webhook.WebhookResponseDto;
import blockmon.repository.SubscribedTxRepository;
import blockmon.service.TransactionObjService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import blockmon.entity.SubscribedTxEntity;
import blockmon.entity.TransactionObjEntity;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.list.MutableList;
import org.eclipse.collections.api.map.MutableMap;
import org.eclipse.collections.impl.list.mutable.FastList;
import org.eclipse.collections.impl.map.mutable.UnifiedMap;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Map;

@Service
@AllArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class TransmissionServiceImpl implements TransmissionService {

    private final TransmissionAdapter transmissionAdapter;
    private final TransactionObjService transactionObjService;
    private final SubscribedTxRepository subscribedTxRepository;
    private final ObjectMapper objectMapper;


    @Override
    @Transactional(noRollbackFor = {Exception.class})
    public void processTransmissions(MutableList<SubscribedTxEntity> pendingTransmissionList) {
        MutableList<TransactionObjEntity> txObjs = FastList.newList(transactionObjService.getTransactions(pendingTransmissionList.collect(e -> e.getRefId())));

        for (SubscribedTxEntity pt : pendingTransmissionList) {
            TransactionObjEntity tObj = txObjs.detect(e -> e.getRefId().equals(pt.getRefId()));
            WebhookResponseDto dto = WebhookResponseDto.builder().
                    refId(pt.getRefId()).txHash(pt.getTxHash())
                    .txStatus(TxStatusDto.toDto(pt.getTxStatus()))
                    .network(pt.getNetwork())
                    .build();

            try {
                MutableMap<String, Object> txDetails = UnifiedMap.newMap(objectMapper.readValue(tObj.getTxResponse(), Map.class));
                dto.setBlockHash(String.valueOf(txDetails.getIfAbsentValue("block_hash",txDetails.get("block"))));
                dto.setBlockHeight((Integer) txDetails.get("block_height"));
                dto.setFee(String.valueOf(txDetails.getIfAbsentValue("fee",txDetails.get("fees"))));
                dto.setEpochSlot((Integer) txDetails.getIfAbsentValue("epoch_slot",txDetails.get("slot")));
                dto.setTxBlockIndex((Integer) txDetails.getIfAbsentValue("tx_block_index",txDetails.get("index")));
                dto.setInvalidBefore(
                        (Integer) txDetails.getIfAbsentValue(
                                "invalid_before",
                                Integer.valueOf(String.valueOf(txDetails.getIfAbsentValue("invalidBefore",0)))));
                dto.setInvalidAfter(
                        (Integer) txDetails.getIfAbsentValue(
                                "invalid_after",
                                Integer.valueOf(String.valueOf(txDetails.getIfAbsentValue("invalidHereafter",0)))));
                dto.setTxSize((Integer) txDetails.getIfAbsentValue("tx_size",txDetails.getIfAbsentValue("size",0)));
                dto.setDeposit(String.valueOf(txDetails.get("deposit")));
                dto.setFullTxDetailsWithOptionalFields(txDetails);
            } catch (JsonProcessingException e) {
                log.error("Error while reading String to Map", e);
            }

            String endpoint = pt.getWebhookUrl();
            HttpStatus httpStatus = null;
            try {
                httpStatus = transmissionAdapter.transmit(dto, endpoint);
            } catch (Exception e) {
                log.error("Exception caught while transmitting for tx hash: " + pt.getTxHash());
                log.error(e.getMessage(), e);
            } finally {
                if (httpStatus != null && httpStatus.is2xxSuccessful()) {
                    pt.setIsWebhookTransmitted(true);
                }
                pt.setTransmissionResponseCode(httpStatus == null ? null : httpStatus.value());
                pt.setCountTransmissionAttempt(pt.getCountTransmissionAttempt() + 1);
                pt.setWebhookTransmitTime(OffsetDateTime.now());
                subscribedTxRepository.saveAll(pendingTransmissionList);
            }
        }
    }
}
