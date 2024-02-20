package blockmon.service;


import blockmon.dto.webhook.WebhookResponseDto;

public interface MockWebhookReceiptService {

    boolean persistPayload(WebhookResponseDto payload);

}