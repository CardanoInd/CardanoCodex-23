package blockmon.repository;

import blockmon.entity.MockWebhookReceiptEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface MockWebhookReceiptRepository extends JpaRepository<MockWebhookReceiptEntity, Long> {

}