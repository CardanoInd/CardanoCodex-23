package blockmon.dto.webhook;

import blockmon.entity.TxStatusEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TxStatusDto {

    String id;
    String description;

    public static TxStatusDto toDto(TxStatusEntity txStatus) {
        return builder().id(txStatus.getId()).description(txStatus.getDescription()).build();
    }
}
