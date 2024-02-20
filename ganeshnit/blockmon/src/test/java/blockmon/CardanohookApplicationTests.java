package blockmon;

import com.fasterxml.jackson.databind.ObjectMapper;
import blockmon.model.SubscriptionRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.json.JacksonJsonParser;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.web.servlet.MockMvc;

import java.util.UUID;

import static blockmon.constants.AppConstants.SUBSCRIBE_TX_URL;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.document;
import static org.springframework.restdocs.operation.preprocess.Preprocessors.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@TestPropertySource(locations = "classpath:local-test-application.properties")
@Sql({"/data.sql", "/test-data.sql"})
public class CardanohookApplicationTests {

    protected static final long testUserPartyId = 101L;
    protected static final long secondTestUserPartyId = 1010102L;
    protected static final String testUsername = "testUser-XWDOaown@gmail.com";
    protected static final String testUserEmail = testUsername;
    protected static final String secondTestUsername = "secondTestUser";
    protected static final String testUserPass = "testUserPass"; //"{bcrypt}$2a$10$1SKWjBlnM01l2G28UQ5NpeAK.nEOqlG07xficI6Zc6W7vW/P.6xYe";
    protected static final Long testProjectId1 = 401L;
    protected static final Long testSubscribedTxId1 = 501L;
    protected static final Long testSubscribedTxId2 = 503L;
    protected static final String testHashPreprod1 = "55568b130a26e79f4d7374213d8060cdcd79ce0f3a59c899a4716f95c3ed4c49";
    protected static final String testHash2 = "6909e4d2a33ed1961c24aaa476dac2cd51a27d01289525be75052596e8ddb0b4";
    protected static final String testHash3 = "dc8ecd20ebcb78161570f0d98efc147d154cabfe1058d90d5ee1b4b74b3e9b25";

    protected static final String testMainnetHash1 = "075e00bffef2bb40afd06b5d1b356211d36d55ca08244e30f3be005ac8b2c837";
    protected static final String testMainnetHash2 = "eddc6056003e5ad33b2c569922634dc25ccad5e01ba73c9c070784f7bcb6ebe0";
    protected static final String inMemTestHashMainnet1 = "3d5bc3c5af2d314bdcbfb19c4fa09054a072e8488405a4fae6d1ac8f275428fc";
    protected static final String inMemTestHashMainnet2 = "6e23e078f4ce82b5231e4d4bc784379aa5acf1e1ed3607e92ebe7de51c12ca81";
    protected static final String inMemTestHashMainnet3 = "8de12bb3f3855a1e807c31f70c795658ba38ff8eed4287f91ee2b017d7baa19a";
    protected static final String inMemTestHashPreprod1 = "839cd4f5548ece6acaaabfc19f5d026f2488f091d689ae4565e004e844ec20d3";
    protected static final String inMemTestHashPreprod2 = "9fe243546fad880eb6f03400a6a31378f396525e04d6756627da81ea7e7a6690";

    protected static final String testnet = "testnet";
    protected static final String testRefId = "01cf3a36-2db1-4d45-853a-fc1ce3bdbf30";
    protected JacksonJsonParser jsonParser = new JacksonJsonParser();
    @Autowired
    protected ObjectMapper objectMapper;
    @Autowired
    protected
    AppConfig appConfig;

    protected static String asJsonString(final Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    protected String randomEmail() {
        return randomString() + "@gmail.com";
    }

    protected String randomString() {
        return UUID.randomUUID().toString();
    }

    protected String randomUrl() {
        return "https://" + randomEmail() + ".png";
    }

    protected void createTxSubscription(SubscriptionRequest subRequest1, MockMvc mockMvc) throws Exception {
        mockMvc.perform(
                        put(SUBSCRIBE_TX_URL)
                                .contentType(MediaType.APPLICATION_JSON)
                                .accept(MediaType.APPLICATION_JSON)
                                .content(asJsonString(subRequest1))
                )
                .andExpect(status().isCreated())
                .andDo(document("{class-name}-{method-name}",
                        preprocessRequest(prettyPrint()), preprocessResponse(prettyPrint())))
                .andReturn();
    }
}
