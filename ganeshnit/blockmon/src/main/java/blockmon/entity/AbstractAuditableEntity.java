package blockmon.entity;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import java.time.OffsetDateTime;

@MappedSuperclass
@Setter
@Getter
public class AbstractAuditableEntity {
    @Column(name = "created_stamp", nullable = false, updatable = false)
    @CreatedDate
    private OffsetDateTime createdDate;

    @Column(name = "last_updated_stamp")
    @LastModifiedDate
    private OffsetDateTime modifiedDate;

    @PrePersist
    protected void onCreation() {
        createdDate = OffsetDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        modifiedDate = OffsetDateTime.now();
    }

}
