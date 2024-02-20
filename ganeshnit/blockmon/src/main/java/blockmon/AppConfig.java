package blockmon;

import blockmon.repository.QueryTargetRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import blockmon.entity.QueryTargetEntity;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.collections.api.map.MutableMap;
import org.eclipse.collections.api.set.MutableSet;
import org.eclipse.collections.impl.map.mutable.UnifiedMap;
import org.eclipse.collections.impl.set.mutable.UnifiedSet;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.json.Jackson2JsonDecoder;
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import org.springframework.web.reactive.function.client.WebClient;
import rest.koios.client.backend.api.transactions.TransactionsService;
import rest.koios.client.backend.factory.BackendFactory;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import static blockmon.constants.Constants.*;

@Configuration
@Getter
@RequiredArgsConstructor
@Slf4j
public class AppConfig {

    public static int QUERY_THREAD_TIMEOUT_MINUTES = 1;
    public static long SECONDS_WAIT_BEFORE_QUERY = 5L;
    public static String DEFAULT_QT = QUERY_TARGET_KOIOS;
    @Value("${mock.test.server.port}")
    private String mockTestServerPort;
    @Value("${preprod.blockfrost.baseurl}")
    private String bfPreprodBaseUrl;
    @Value("${preprod.blockfrost.api.key}")
    private String bfPreprodApiKey;
    @Value("${mainnet.blockfrost.baseurl}")
    private String bfMainnetBaseUrl;
    @Value("${mainnet.blockfrost.api.key}")
    private String bfMainnetApiKey;

    @Value("${webhook.transmission.retry.limit}")
    private Integer transmissionRetryLimit;

    private MutableSet<String> allowedQueryTargetSet = UnifiedSet.newSetWith(
            QUERY_TARGET_KOIOS, QUERY_TARGET_BLOCKFROST
    );

    private final QueryTargetRepository queryTargetRepository;

    @Bean
    public String getMockWebhookEndpoint() {
        return "http://localhost:" + mockTestServerPort + "/api/v1/mock/webhook-receiver";
    }

    @Bean
    public MutableSet<String> allowedQueryTargetSet() {
        return UnifiedSet.newSetWith(
                QUERY_TARGET_KOIOS, QUERY_TARGET_BLOCKFROST
        );
    }

    @Bean
    public MutableSet<String> allowedNetworkNames() {
        return UnifiedSet.newSetWith(
                PREPROD, TESTNET, MAINNET
        );
    }

    @Bean
    public MutableMap<String, QueryTargetEntity> queryTargetEntityCache() {
        return UnifiedMap.newWithKeysValues(
                QUERY_TARGET_KOIOS, queryTargetRepository.getReferenceById(QUERY_TARGET_KOIOS),
                QUERY_TARGET_BLOCKFROST, queryTargetRepository.getReferenceById(QUERY_TARGET_BLOCKFROST)
        );
    }

    @Bean
    public ObjectMapper objectMapper() {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        return objectMapper;
    }

    @Bean(value = "koiosMainnetClient")
    public TransactionsService koiosMainnetClient() {
        return BackendFactory.getKoiosMainnetService().getTransactionsService();
    }

    @Bean(value = "blockfrostPreprodClient")
    public WebClient blockfrostPreprodWebClient() {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        objectMapper.setPropertyNamingStrategy(PropertyNamingStrategies.SNAKE_CASE);

        ExchangeStrategies exchangeStrategies = ExchangeStrategies.builder()
                .codecs(configurer ->
                        configurer.defaultCodecs().jackson2JsonDecoder(new Jackson2JsonDecoder(objectMapper)))
                .build();
        String baseUrl = bfPreprodBaseUrl;
        String apiKey = bfPreprodApiKey;
        return WebClient.builder()
                .baseUrl(baseUrl)
                .defaultHeader("project_id", apiKey)
                .defaultHeader("Accept", "application/json")
                .exchangeStrategies(exchangeStrategies)
                .build();
    }

    @Bean(value = "blockfrostMainnetClient")
    public WebClient blockfrostMainnetWebClient() {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        objectMapper.setPropertyNamingStrategy(PropertyNamingStrategies.SNAKE_CASE);

        ExchangeStrategies exchangeStrategies = ExchangeStrategies.builder()
                .codecs(configurer ->
                        configurer.defaultCodecs().jackson2JsonDecoder(new Jackson2JsonDecoder(objectMapper)))
                .build();
        String baseUrl = bfMainnetBaseUrl;
        String apiKey = bfMainnetApiKey;
        return WebClient.builder()
                .baseUrl(baseUrl)
                .defaultHeader("project_id", apiKey)
                .defaultHeader("Accept", "application/json")
                .exchangeStrategies(exchangeStrategies)
                .build();
    }

    @Bean(value = "transmissionClient")
    public WebClient transmissionWebClient() {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        objectMapper.setPropertyNamingStrategy(PropertyNamingStrategies.SNAKE_CASE);

        ExchangeStrategies exchangeStrategies = ExchangeStrategies.builder()
                .codecs(configurer ->
                        configurer.defaultCodecs().jackson2JsonDecoder(new Jackson2JsonDecoder(objectMapper)))
                .build();
        return WebClient.builder()
                .defaultHeader("Accept", "application/json")
                .exchangeStrategies(exchangeStrategies)
                .build();
    }

    @Bean("blockfrostExecutor")
    Executor blockfrostExecutor() {
        return Executors.newFixedThreadPool(3);
    }

    @Bean("koiosExecutor")
    Executor koiosExecutor() {
        return Executors.newFixedThreadPool(1);
    }

    public void setDefaultQueryTarget(String newValue) {
        if (newValue != QUERY_TARGET_BLOCKFROST || newValue != QUERY_TARGET_KOIOS) {
            return;
        }
        DEFAULT_QT = newValue;
    }

    public void setQueryThreadTimeoutMinutes(int newValue) {
        if (newValue > 0) {
            QUERY_THREAD_TIMEOUT_MINUTES = newValue;
        }
    }

    public void setSecondsWaitBeforeQuery(int newValue) {
        if (newValue > 0) {
            SECONDS_WAIT_BEFORE_QUERY = newValue;
        }
    }
}
