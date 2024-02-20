package blockmon.entity;

import lombok.*;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.validator.constraints.URL;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import java.time.OffsetDateTime;


@Entity
@Table(name = "subscribed_tx")
@SequenceGenerator(name = "subscribed_tx_gen", sequenceName = "subscribed_tx_seq", initialValue = 10000)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SubscribedTxEntity extends AbstractAuditableEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "subscribed_tx_gen")
    @Column(name = "id", updatable = false, nullable = false)
    private Long id;

    @Column(name = "ref_id", updatable = false, nullable = false)
    private String refId;

    @NotEmpty(message = "Tx hash cannot be empty.")
    @Basic(optional = false)
    @Column(name = "tx_hash", updatable = false)
    private String txHash;

    @Column(name = "network")
    private String network;

    @ManyToOne()
    @JoinColumn(name = "status_id", nullable = false)
    @Fetch(FetchMode.JOIN)
    private TxStatusEntity txStatus;

    @ManyToOne
    @JoinColumn(name = "query_target", nullable = false)
    @Fetch(FetchMode.JOIN)
    private QueryTargetEntity queryTarget;

    @URL
    @Column(name = "webhook_url", length = 2048)
    private String webhookUrl;

    @Builder.Default
    private Boolean isWebhookTransmitted = false;
    private OffsetDateTime webhookTransmitTime;
    @Builder.Default
    private Integer countTransmissionAttempt = 0;
    private Integer transmissionResponseCode;

}
