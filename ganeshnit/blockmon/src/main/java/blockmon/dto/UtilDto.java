package blockmon.dto;

import org.eclipse.collections.api.map.MutableMap;
import org.eclipse.collections.impl.map.mutable.UnifiedMap;

import java.util.Collections;

import static blockmon.constants.Constants.*;


public class UtilDto {

    //TODO init all statically

    public static MutableMap<String, Object> newPagedResponseMap() {
        MutableMap<String, Object> map = UnifiedMap.newMap();
        map.put(RESULT_LIST, Collections.emptyList());
        map.put(LIST_SIZE, 0);
        map.put(TOTAL_PAGES, 0);
        map.put(HAS_NEXT_PAGE, false);
        return map;
    }

    public static MutableMap<String, Object> newSingleItemResponseMap() {
        MutableMap<String, Object> map = UnifiedMap.newMap();
        map.put(RESULT_ITEM, Collections.emptyList());
        return map;
    }
}
