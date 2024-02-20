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

import static blockmon.constants.AppConstants.SUBSCRIBE_TX_URL;
import static blockmon.constants.Constants.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.documentationConfiguration;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith({MockitoExtension.class, RestDocumentationExtension.class, SpringExtension.class})
public class MockControllerIT extends CardanohookApplicationTests {

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
    public void givenValidApiKey_whenRegisterTx_thenOk() throws Exception {
        MockHttpServletResponse response = mockMvc.perform(
                        put(SUBSCRIBE_TX_URL)
                                .contentType(MediaType.APPLICATION_JSON)
                                .accept(MediaType.APPLICATION_JSON)
                                .param(NETWORK, testnet)
                                .param(TX_HASH, testHashPreprod1)
                )
                .andExpect(status().isCreated())
                .andReturn().getResponse();
        Map<String, Object> map = jsonParser.parseMap(response.getContentAsString());
        assertEquals(testHashPreprod1, map.get(TX_HASH));
        assertNotNull(map.get(REF_ID));
        Map<String, Object> statusMap = (Map<String, Object>) map.get("status");
        assertEquals(NOT_CHECKED, statusMap.get(ID));
        assertEquals("Status not checked", statusMap.get(DESCRIPTION));
    }
}
