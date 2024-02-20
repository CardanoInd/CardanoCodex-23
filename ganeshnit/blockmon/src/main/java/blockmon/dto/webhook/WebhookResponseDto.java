package blockmon.dto.webhook;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class WebhookResponseDto {

    String refId;
    TxStatusDto txStatus;
    String network;

    String txHash;
    private String blockHash;
    private Integer blockHeight;
    private Integer epochSlot;
    private Integer txBlockIndex;
    private String fee;
    private Integer invalidBefore;
    private Integer invalidAfter;
    private Integer txSize;
    private String deposit;
    private Map<String, Object> fullTxDetailsWithOptionalFields;
}
