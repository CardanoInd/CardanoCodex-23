package blockmon.repository;

import blockmon.entity.TransactionObjEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;


public interface TransactionObjectRepository extends JpaRepository<TransactionObjEntity, Long> {
    boolean existsByTxHash(String txHash);

    Optional<TransactionObjEntity> findByTxHash(String txHash);

    Optional<TransactionObjEntity> findByRefId(String refId);

    List<TransactionObjEntity> findAllByRefIdIn(List<String> refIds);

    Optional<TransactionObjEntity> deleteByTxHash(String txHash);

}