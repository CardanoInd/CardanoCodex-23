package blockmon.controller;

import blockmon.dto.SubscribedTxDto;
import blockmon.model.SubscriptionRequest;
import blockmon.service.SubscriptionService;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

import static org.springframework.http.ResponseEntity.status;

@RestController
@RequestMapping(path = "/api/v1/subscription",
        produces = "application/json")
@CrossOrigin(origins = "*")
@AllArgsConstructor
public class SubscriptionController {
    private final SubscriptionService service;

    @PutMapping("/register-tx")
    public ResponseEntity<SubscribedTxDto> registerTx(
            @RequestBody @Valid SubscriptionRequest subscriptionRequest) {
        return status(HttpStatus.CREATED)
                .body(service.registerTx(subscriptionRequest));
    }

}
