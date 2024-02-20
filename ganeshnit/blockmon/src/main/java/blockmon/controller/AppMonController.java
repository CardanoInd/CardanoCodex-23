package blockmon.controller;

import lombok.AllArgsConstructor;
import org.eclipse.collections.api.map.MutableMap;
import org.eclipse.collections.impl.map.mutable.UnifiedMap;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static org.springframework.http.ResponseEntity.ok;

@RestController
@RequestMapping(path = "/api/v1/appmon",
        produces = "application/json")
@CrossOrigin(origins = "*")
@AllArgsConstructor
public class    AppMonController {

    @GetMapping("/ping")
    public ResponseEntity ping() {
        return ok("Cardanohook says, \"Hello world\"!");
    }

    @GetMapping("memory-status")
    public ResponseEntity<MutableMap<String, Object>> getMemoryStatistics() {
        MutableMap<String, Object> stats = UnifiedMap.newMap();
        stats.put("heapSize", Runtime.getRuntime().totalMemory() / (1024 * 1024));
        stats.put("heapMaxSize", Runtime.getRuntime().maxMemory() / (1024 * 1024));
        stats.put("heapFreeSize", Runtime.getRuntime().freeMemory() / (1024 * 1024));
        return ok(stats);
    }

}
