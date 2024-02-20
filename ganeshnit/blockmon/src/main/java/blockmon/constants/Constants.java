package blockmon.constants;

import org.springframework.context.annotation.Configuration;

@Configuration
public class Constants {

    public static final String RESULT_LIST = "resultList";
    public static final String RESULT_ITEM = "resultItem";
    public static final String LIST_SIZE = "listSize";
    public static final String TOTAL_SIZE = "totalSize";
    public static final String TOTAL_PAGES = "totalPages";
    public static final String HAS_NEXT_PAGE = "hasNextPage";

    //Blockfrost
    public static final String TX_UTXOS_ENDPOINT = "/txs/{hash}/utxos";

    public static final String LOVELACE = "lovelace";
    public static final String TESTNET = "testnet";
    public static final String PREPROD = "preprod";
    public static final String MAINNET = "mainnet";
    public static final String NETWORK = "network";
    public static final String TX_HASH = "txHash";
    public static final String REF_ID = "refId";
    public static final String ID = "id";
    public static final String DESCRIPTION = "description";
    public static final String NOT_CHECKED = "NOT_CHECKED";
    public static final String TX_ON_CHAIN = "TX_ON_CHAIN";
    public static final String TX_PENDING = "TX_PENDING";
    public static final String TX_PROCESS_ERROR = "TX_PROCESS_ERROR";

    public static final String QUERY_TARGET_BLOCKFROST = "QT_BLOCKFROST";
    public static final String QUERY_TARGET_DANDELION = "QT_DANDELION";
    public static final String QUERY_TARGET_KOIOS = "QT_KOIOS";

    public static final String BLOCKFROST_TX_ENDPOINT = "/txs/{hash}";

}
