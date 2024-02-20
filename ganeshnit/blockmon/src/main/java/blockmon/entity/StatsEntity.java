package blockmon.entity;

import lombok.*;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Table(name = "app_stats")
@Entity
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StatsEntity {

    @Id
    @Column(name = "stat_id", nullable = false, length = 64)
    private String statId;

    @Column(name = "description")
    private String description;

    @Column(name = "stat_value")
    private String statValue;

}

