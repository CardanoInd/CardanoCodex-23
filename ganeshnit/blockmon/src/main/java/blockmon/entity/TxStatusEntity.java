package blockmon.entity;

import lombok.*;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "tx_status")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TxStatusEntity extends AbstractAuditableEntity {
    @Id
    @Column(name = "id", updatable = false, nullable = false)
    private String id;

    @Column(name = "description", updatable = false, nullable = false)
    private String description;
}