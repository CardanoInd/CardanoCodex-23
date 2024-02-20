package blockmon.controller;

import blockmon.dto.webhook.WebhookResponseDto;
import blockmon.service.MockWebhookReceiptService;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

import static org.springframework.http.ResponseEntity.internalServerError;
import static org.springframework.http.ResponseEntity.ok;

@RestController
@RequestMapping(path = "/api/v1/mock",
        produces = "application/json")
@CrossOrigin(origins = "*")
@AllArgsConstructor
public class MockController {
    private final MockWebhookReceiptService service;

    @PostMapping("/webhook-receiver")
    public ResponseEntity<?> receiveWebhook(
            @RequestBody @Valid WebhookResponseDto payload) {
        boolean b = service.persistPayload(payload);
        return b ? ok().build() : internalServerError().build();
    }

}
