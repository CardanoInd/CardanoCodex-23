package blockmon.controller;

import blockmon.CardanohookApplicationTests;
import blockmon.repository.SubscribedTxRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.json.JacksonTester;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.restdocs.RestDocumentationContextProvider;
import org.springframework.restdocs.RestDocumentationExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import java.util.Map;
import java.util.Properties;

import static blockmon.constants.AppConstants.SUBSCRIBE_TX_URL;
import static blockmon.constants.Constants.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.document;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.documentationConfiguration;
import static org.springframework.restdocs.operation.preprocess.Preprocessors.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith({MockitoExtension.class, RestDocumentationExtension.class, SpringExtension.class})
public class SubscriptionControllerIT extends CardanohookApplicationTests {

    @Autowired
    SubscribedTxRepository repository;
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

    @Test
    public void whenRegisterTxPreprod_thenOk() throws Exception {
        Properties reqBody = new Properties();
        reqBody.put("txHash", testHashPreprod1);
        reqBody.put("network", PREPROD);
        reqBody.put("webhookUrl", appConfig.getMockWebhookEndpoint());

        MockHttpServletResponse response = mockMvc.perform(
                        put(SUBSCRIBE_TX_URL)
                                .contentType(MediaType.APPLICATION_JSON)
                                .accept(MediaType.APPLICATION_JSON)
                                .content(asJsonString(reqBody))
                )
                .andExpect(status().isCreated())
                .andDo(document("{class-name}-{method-name}",
                        preprocessRequest(prettyPrint()), preprocessResponse(prettyPrint())))
                .andReturn().getResponse();
        Map<String, Object> map = jsonParser.parseMap(response.getContentAsString());
        assertEquals(testHashPreprod1, map.get(TX_HASH));
        assertNotNull(map.get(REF_ID));
    }
    @Test
    public void whenRegisterTxTestnet_thenOk() throws Exception {
        Properties reqBody = new Properties();
        reqBody.put("txHash", testHash2);
        reqBody.put("network", testnet);
        reqBody.put("webhookUrl", appConfig.getMockWebhookEndpoint());

        MockHttpServletResponse response = mockMvc.perform(
                        put(SUBSCRIBE_TX_URL)
                                .contentType(MediaType.APPLICATION_JSON)
                                .accept(MediaType.APPLICATION_JSON)
                                .content(asJsonString(reqBody))
                )
                .andExpect(status().isCreated())
                .andReturn().getResponse();
        Map<String, Object> map = jsonParser.parseMap(response.getContentAsString());
        assertEquals(testHash2, map.get(TX_HASH));
        assertNotNull(map.get(REF_ID));
    }
}
