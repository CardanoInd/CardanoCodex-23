package blockmon.entity;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Table(name = "enumeration")
@Entity
@Getter
@Setter
public class EnumerationEntity {
    @Id
    @Column(name = "enum_id", nullable = false, length = 20)
    private String enumId;

    @Column(name = "enum_type", nullable = false, length = 20)
    private String enumType;

    @Column(name = "description")
    private String description;

}