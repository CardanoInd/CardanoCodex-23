package blockmon.repository;

import blockmon.entity.QueryTargetEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface QueryTargetRepository extends JpaRepository<QueryTargetEntity, String> {
}
