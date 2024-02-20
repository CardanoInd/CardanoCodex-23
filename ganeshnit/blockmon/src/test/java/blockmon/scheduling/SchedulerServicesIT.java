package blockmon.scheduling;

import blockmon.AppConfig;
import blockmon.CardanohookApplicationTests;
import blockmon.entity.SubscribedTxEntity;
import blockmon.entity.TransactionObjEntity;
import blockmon.model.SubscriptionRequest;
import blockmon.repository.SubscribedTxRepository;
import blockmon.repository.TransactionObjectRepository;
import lombok.extern.slf4j.Slf4j;
import org.awaitility.Awaitility;
import org.awaitility.Durations;
import org.eclipse.collections.api.list.MutableList;
import org.eclipse.collections.impl.list.mutable.FastList;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.json.JacksonTester;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.restdocs.RestDocumentationContextProvider;
import org.springframework.restdocs.RestDocumentationExtension;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.List;

import static blockmon.constants.Constants.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.documentationConfiguration;

@ExtendWith({MockitoExtension.class, RestDocumentationExtension.class, SpringExtension.class})
@TestPropertySource(properties = {
        "cron.expression.query.tx.status=10/20 * * * * ?",
        "cron.expression.transmit.webhook=59/30 * * * * ?"
})
@Slf4j
public class SchedulerServicesIT extends CardanohookApplicationTests {

    @Autowired
    SubscribedTxRepository subscribedTxRepository;
    @Autowired
    TransactionObjectRepository transactionObjectRepository;
    @Autowired
    AppConfig spyAppConfig;
    @Autowired
    private WebApplicationContext context;
    private MockMvc mockMvc;

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

    @Test
    public void givenQueryScheduled_whenNewSubTxs_thenQueriedAndStatusUpdated()
            throws Exception {
        setAppConfigValues();
        transactionObjectRepository.deleteAll();
        subscribedTxRepository.deleteAll();
        SubscriptionRequest subRequest1 = new SubscriptionRequest(inMemTestHashMainnet1, MAINNET, appConfig.getMockWebhookEndpoint());
        SubscriptionRequest subRequest2 = new SubscriptionRequest(inMemTestHashPreprod1, TESTNET, appConfig.getMockWebhookEndpoint());
        SubscriptionRequest subRequest3 = new SubscriptionRequest(inMemTestHashPreprod2, TESTNET, appConfig.getMockWebhookEndpoint());
        SubscriptionRequest subRequest4 = new SubscriptionRequest(inMemTestHashMainnet2, MAINNET, appConfig.getMockWebhookEndpoint());
        SubscriptionRequest subRequest5 = new SubscriptionRequest(inMemTestHashMainnet3, MAINNET, appConfig.getMockWebhookEndpoint());

        createTxSubscription(subRequest1, mockMvc);
        createTxSubscription(subRequest2, mockMvc);
        createTxSubscription(subRequest3, mockMvc);
        createTxSubscription(subRequest4, mockMvc);
        createTxSubscription(subRequest5, mockMvc);

        long inDBCount = subscribedTxRepository.count();
        assertEquals(5, inDBCount);

        MutableList<SubscribedTxEntity> subTxs = FastList.newList(subscribedTxRepository.findAll());
        assertTrue(subTxs.allSatisfy(e -> !e.getIsWebhookTransmitted()));
        assertTrue(subTxs.allSatisfy(e -> e.getCountTransmissionAttempt() == 0));

        log.info("Should not get queried up until 10 seconds");
        Awaitility.await()
                .pollDelay(Duration.of(1L, ChronoUnit.SECONDS))
                .pollInterval(Durations.FIVE_SECONDS)
                .atMost(Durations.ONE_MINUTE)
                .untilAsserted(() -> assertEquals(5, countStatusUpdated()));

        subTxs = FastList.newList(subscribedTxRepository.findAll());
        assertTrue(subTxs.allSatisfy(e -> !e.getIsWebhookTransmitted()));
        /*assertEquals(0,subTxs.get(0).getCountTransmissionAttempt());
        assertEquals(0,subTxs.get(1).getCountTransmissionAttempt());*/
        MutableList<SubscribedTxEntity> preprodSubs = subTxs.select(e -> e.getNetwork().equals(PREPROD));
        assertTrue(preprodSubs.allSatisfy(e->e.getQueryTarget().getId().equals(QUERY_TARGET_BLOCKFROST)));
        MutableList<SubscribedTxEntity> mainnetSubs = subTxs.select(e -> e.getNetwork().equals(MAINNET));
        assertTrue(mainnetSubs.allSatisfy(e->e.getQueryTarget().getId().equals(QUERY_TARGET_KOIOS)));

        List<TransactionObjEntity> txObjs = transactionObjectRepository.findAll();
        assertEquals(5, txObjs.size());

        log.info("Awaiting transmission");
        Awaitility.await()
                .pollDelay(Duration.of(10L, ChronoUnit.SECONDS))
                .pollInterval(Durations.FIVE_SECONDS)
                .atMost(Durations.ONE_MINUTE)
                .untilAsserted(() -> assertEquals(5, countTransmissionAttemptedAtleastOnce()));
    }

    private long countTransmissionAttemptedAtleastOnce() {
        return subscribedTxRepository.countByCountTransmissionAttemptGreaterThan(0);
    }

    private long countStatusUpdated() {
        return subscribedTxRepository.findAllByTxStatusId(TX_ON_CHAIN).stream().count();
    }

    private void setAppConfigValues() {
        appConfig.setSecondsWaitBeforeQuery(10);
    }
}
