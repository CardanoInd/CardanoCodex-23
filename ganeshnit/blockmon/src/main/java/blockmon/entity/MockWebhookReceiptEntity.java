package blockmon.entity;

import lombok.*;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;


@Entity
@Table(name = "mock_webhook_receipt")
@SequenceGenerator(name = "mock_wr_gen", sequenceName = "mock_wr_seq", initialValue = 10000)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MockWebhookReceiptEntity extends AbstractAuditableEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "mock_wr_gen")
    @Column(name = "id", updatable = false, nullable = false)
    private Long id;

    @Column(name = "ref_id", updatable = false, nullable = false)
    private String refId;

    @NotEmpty(message = "Tx hash cannot be empty.")
    @Basic(optional = false)
    @Column(name = "tx_hash", updatable = false)
    private String txHash;

    @ManyToOne()
    @JoinColumn(name = "status_id", nullable = false)
    private TxStatusEntity txStatus;


}
