package blockmon.adapter.blockfrost;

import blockmon.adapter.BlockfrostQueryAdapter;
import blockmon.constants.Constants;
import blockmon.exception.BlockfrostAdaptorException;
import blockmon.exception.ErrorCode;
import blockmon.model.blockfrost.Transaction;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

@Service
@Slf4j
public class BlockfrostAdapterMainnet implements BlockfrostQueryAdapter {

    private WebClient blockfrostWebClient;

    public BlockfrostAdapterMainnet(@Qualifier("blockfrostMainnetClient") WebClient blockfrostWebClient) {
        this.blockfrostWebClient = blockfrostWebClient;
    }

    @Transactional(noRollbackFor = {BlockfrostAdaptorException.class})
    public Transaction checkTxStatus(String hash) {

        if (hash == null || hash.isEmpty()) {
            return null;
        }

        log.info("Issuing blockfrost query for hash: " + hash);
        WebClient.ResponseSpec responseSpec = blockfrostWebClient
                .get()
                .uri(Constants.BLOCKFROST_TX_ENDPOINT.replaceFirst("\\{hash\\}", hash))
                .retrieve();
        Transaction txResponse = null;
        try {
            txResponse = responseSpec
                    .onStatus(
                            HttpStatus.TOO_MANY_REQUESTS::equals,
                            response -> {
                                log.error("Blockfrost project usage is over limit.");
                                return Mono.error(new BlockfrostAdaptorException(
                                                "Blockfrost project usage is over limit.",
                                                ErrorCode.BLOCKFROST_TOO_MANY_REQUESTS,
                                                HttpStatus.TOO_MANY_REQUESTS
                                        )
                                );
                            })
                    .onStatus(
                            HttpStatus::isError,
                            response -> {
                                log.error("hash : " + hash + ", Response : " + response.statusCode());
                                return Mono.error(new BlockfrostAdaptorException(
                                                "Error from blockfrost.",
                                                ErrorCode.BLOCKFROST_ERROR,
                                                response.statusCode()
                                        )
                                );
                            })
                    .bodyToMono(Transaction.class).block();
        } catch (RuntimeException e) {
            log.error("Error while querying blockfrost - ", e);
            throw e;
        }


        return txResponse;

    }

}