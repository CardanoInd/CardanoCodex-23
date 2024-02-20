package blockmon.adapter;

import blockmon.dto.webhook.WebhookResponseDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;

@Service
@Slf4j
public class TransmissionAdapter {

    private final WebClient webClient;

    public TransmissionAdapter(@Qualifier("transmissionClient") WebClient webClient) {
        this.webClient = webClient;
    }

    public HttpStatus transmit(WebhookResponseDto dto, String fqEndpoint) {

        if (fqEndpoint == null || fqEndpoint.isEmpty()) {
            return null;
        }

        log.info("Transmitting to webhook for hash: " + dto.getTxHash());
        //WebClient.ResponseSpec responseSpec =
        HttpStatus httpStatus = webClient
                .post()
                .uri(fqEndpoint)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .body(BodyInserters.fromValue(dto))
                .retrieve()
                .toBodilessEntity()
                .block()
                .getStatusCode();

        return httpStatus;

    }

}