package blockmon.service;

import blockmon.CardanohookApplicationTests;
import blockmon.entity.SubscribedTxEntity;
import blockmon.repository.SubscribedTxRepository;
import blockmon.repository.TransactionObjectRepository;
import blockmon.service.query.BlockfrostQueryService;
import blockmon.service.webhook.TransmissionService;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.impl.list.mutable.FastList;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.json.JacksonTester;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.restdocs.RestDocumentationContextProvider;
import org.springframework.restdocs.RestDocumentationExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import java.util.concurrent.Future;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.documentationConfiguration;

@ExtendWith({MockitoExtension.class, RestDocumentationExtension.class, SpringExtension.class})
@Slf4j
public class TransmissionServiceIT extends CardanohookApplicationTests {

    @Autowired
    TransmissionService transmissionService;
    @Autowired
    BlockfrostQueryService blockfrostQueryService;
    @Autowired
    TransactionObjectRepository transactionObjectRepository;
    @Autowired
    SubscribedTxRepository subscribedTxRepository;
    private MockMvc mockMvc;
    @Autowired
    private WebApplicationContext context;

    @BeforeEach
    public void setup(RestDocumentationContextProvider restDocumentation) {
        JacksonTester.initFields(this, objectMapper);
        MappingJackson2HttpMessageConverter mappingJackson2HttpMessageConverter = new
                MappingJackson2HttpMessageConverter();
        mappingJackson2HttpMessageConverter.setObjectMapper(objectMapper);
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)

                .apply(documentationConfiguration(restDocumentation))
                .build();

    }

    @Test //TO BE RUN WITH A LIVE INSTANCE RUNNING TO RECEIVE WEBHOOKS
    public void givenCheckedTx_whenTransmit_thenMockWebhookReceives() throws Exception {

        SubscribedTxEntity subscribedTxEntity = subscribedTxRepository.findById(testSubscribedTxId1).orElseThrow();
        assertEquals(false, subscribedTxEntity.getIsWebhookTransmitted());
        assertEquals(0, subscribedTxEntity.getCountTransmissionAttempt());
        assertNull(subscribedTxEntity.getWebhookTransmitTime());
        assertNull(subscribedTxEntity.getTransmissionResponseCode());

        Future<Boolean> future = blockfrostQueryService.queryAndUpdateSubscribedTx(FastList.newListWith(subscribedTxEntity));

        while (true) {
            if (future.isDone()) {
                log.info("queryAndUpdate completed");
                break;
            }
            log.error("Continue doing something else. ");
            Thread.sleep(1000);
        }

        transactionObjectRepository.findByRefId(subscribedTxEntity.getRefId()).orElseThrow();
        subscribedTxEntity = subscribedTxRepository.findById(testSubscribedTxId1).orElseThrow();

        transmissionService.processTransmissions(FastList.newListWith(subscribedTxEntity));

        subscribedTxEntity = subscribedTxRepository.findById(testSubscribedTxId1).orElseThrow();
        assertEquals(true, subscribedTxEntity.getIsWebhookTransmitted());
        assertEquals(1, subscribedTxEntity.getCountTransmissionAttempt());
        assertNotNull(subscribedTxEntity.getWebhookTransmitTime());
        assertEquals(HttpStatus.OK.value(), subscribedTxEntity.getTransmissionResponseCode());
    }
}
