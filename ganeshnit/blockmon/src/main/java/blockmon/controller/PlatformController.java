package blockmon.controller;

import blockmon.AppConfig;
import blockmon.entity.SubscribedTxEntity;
import blockmon.repository.QueryTargetRepository;
import blockmon.repository.StatusRepository;
import blockmon.repository.SubscribedTxRepository;
import lombok.AllArgsConstructor;
import org.eclipse.collections.api.block.procedure.Procedure;
import org.eclipse.collections.impl.map.mutable.UnifiedMap;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

import static blockmon.constants.Constants.*;
import static org.springframework.http.ResponseEntity.ok;

@RestController
@RequestMapping(path = "/api/v1/platform/209FCOb-opiFA02nfaqalwiSVN4374W7poZXpwgrZ",
        produces = "application/json")
@CrossOrigin(origins = "*")
@AllArgsConstructor
public class PlatformController {

    private final AppConfig appConfig;
    private final SubscribedTxRepository subscribedTxRepository;
    private final QueryTargetRepository queryTargetRepository;
    private final StatusRepository statusRepository;

    @GetMapping("change-default-qt")
    public ResponseEntity<Boolean> changeDefaultQt(
            @RequestParam("value") String newValue) {

        Boolean result = false;
        switch (newValue) {
            case QUERY_TARGET_BLOCKFROST:
            case QUERY_TARGET_KOIOS:
                appConfig.setDefaultQueryTarget(newValue);
                result = true;
                break;
            default:
                break;
        }
        return ok(result);
    }

    @GetMapping("reset-qt-pending")
    public ResponseEntity<Map<String, Object>> resetQtForPending() {

        List<SubscribedTxEntity> subTxEs = subscribedTxRepository.selectNonTransmitted(appConfig.getTransmissionRetryLimit());
        subTxEs.forEach((Procedure<SubscribedTxEntity>) e -> {
            //TODO optimise fetch from in mem
            e.setQueryTarget(queryTargetRepository.getReferenceById(appConfig.DEFAULT_QT));
            e.setTxStatus(statusRepository.getReferenceById(TX_ON_CHAIN));
        });
        subscribedTxRepository.saveAll(subTxEs);
        return ok(UnifiedMap.newWithKeysValues("updatedCount", subTxEs.size()));
    }

}
