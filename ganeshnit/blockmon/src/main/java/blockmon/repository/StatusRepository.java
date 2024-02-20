package blockmon.repository;

import blockmon.entity.TxStatusEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface StatusRepository extends JpaRepository<TxStatusEntity, String> {
    Optional<TxStatusEntity> findById(String id);
}
