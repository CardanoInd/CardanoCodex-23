package blockmon.entity;

import lombok.*;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;

@Entity
@Table(name = "transaction_obj")
@SequenceGenerator(name = "transaction_obj_gen", sequenceName = "transaction_obj_seq", initialValue = 10000)
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransactionObjEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "transaction_obj_gen")
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

    @ManyToOne
    @JoinColumn(name = "query_target", nullable = false)
    @Fetch(FetchMode.JOIN)
    private QueryTargetEntity queryTarget;

    @Column(name = "tx_response", length = 102400)
    private String txResponse;
}
