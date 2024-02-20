package blockmon.repository;

import blockmon.entity.SubscribedTxEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface SubscribedTxRepository extends JpaRepository<SubscribedTxEntity, Long> {

    Optional<SubscribedTxEntity> findByRefId(UUID uuid);

    Optional<SubscribedTxEntity> findByTxHash(String hash);

    @Query(value = "select * from subscribed_tx t " +
            "where t.status_id <> 'TX_ON_CHAIN' and t.created_stamp < ?1",
            nativeQuery = true)
    List<SubscribedTxEntity> selectNonConfirmedTxs(OffsetDateTime createdBefore);

    @Query(value = "select * from subscribed_tx t " +
            "where t.status_id <> 'TX_ON_CHAIN' and t.query_target = ?1 and t.created_stamp < ?2",
            nativeQuery = true)
    List<SubscribedTxEntity> selectNonConfirmedTxsByQueryTarget(String qtId, OffsetDateTime createdBefore);

    @Query(value = "select stx.*,ts.* from subscribed_tx stx \n" +
            "    join tx_status ts on stx.status_id = ts.id\n" +
            "    join query_target qt on stx.query_target = qt.id\n" +
            "where stx.is_webhook_transmitted = false and stx.status_id = 'TX_ON_CHAIN' and stx.count_transmission_attempt <= ?1\n" +
            "order by last_updated_stamp",
            nativeQuery = true)
    List<SubscribedTxEntity> selectNonTransmitted(Integer retryLimit);

    List<SubscribedTxEntity> findAllByTxStatusId(String statusId);

    long countByCountTransmissionAttemptGreaterThan(int val);
}