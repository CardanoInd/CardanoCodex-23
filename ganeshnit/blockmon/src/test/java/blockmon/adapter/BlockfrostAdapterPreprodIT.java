package blockmon.adapter;

import blockmon.CardanohookApplicationTests;
import blockmon.constants.Constants;
import blockmon.model.blockfrost.Transaction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.json.JacksonTester;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.restdocs.RestDocumentationContextProvider;
import org.springframework.restdocs.RestDocumentationExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.reactive.function.client.WebClient;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.restdocs.mockmvc.MockMvcRestDocumentation.documentationConfiguration;

@ExtendWith({MockitoExtension.class, RestDocumentationExtension.class, SpringExtension.class})
public class BlockfrostAdapterPreprodIT extends CardanohookApplicationTests {

    private MockMvc mockMvc;

    @Autowired
    @Qualifier("blockfrostPreprodClient")
    private WebClient webClient;
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
    public void whenQueryBlockFrostForTransactionUtxo_thenReturnTransactionUtxoForAHash() {

        String hash = "280558165ba59bffe7973036d1a58a1da1f5354dbf74ea04840b482879fd5bbf";

        /*
        https://preprod.cardanoscan.io/transaction/233dce002818f8c4a9dcece2372d33daa76ba018ea53d19c93eec6b79b47ffe0
        input address - addr_test1qzx3xd09g0relx654r70p7dwduqhdq0jlt8mwrxe0y0dehreglsjryy0003yh9enn29g3agxk2kfjduertrhakd75txseyzp0z
        input ada - 461.459849 ₳
        output addr1 - addr_test1qzmutexsfdnzpa74uqr86dx6wakcm300h477pq35la6z6eg0ap90f28nfrt78hz43ynq9lxwvrr6yrdl0sunatfhlmnqatk5t8
        output ada- 5.0 ₳
        output addr1 - addr_test1qzx3xd09g0relx654r70p7dwduqhdq0jlt8mwrxe0y0dehreglsjryy0003yh9enn29g3agxk2kfjduertrhakd75txseyzp0z
        output ada- 456.281896 ₳
        */

        /*String addrSender = "addr_test1qzx3xd09g0relx654r70p7dwduqhdq0jlt8mwrxe0y0dehreglsjryy0003yh9enn29g3agxk2kfjduertrhakd75txseyzp0z";
        String addrRecipient = "addr_test1qzmutexsfdnzpa74uqr86dx6wakcm300h477pq35la6z6eg0ap90f28nfrt78hz43ynq9lxwvrr6yrdl0sunatfhlmnqatk5t8";
        String addrChange = "addr_test1qzx3xd09g0relx654r70p7dwduqhdq0jlt8mwrxe0y0dehreglsjryy0003yh9enn29g3agxk2kfjduertrhakd75txseyzp0z";

        String inputLovelace = String.format("%.0f", 461.459849 * 1000000);
        String outputLovelace = String.format("%.0f", 5.0 * 1000000);
        String changeLovelace = String.format("%.0f", 456.281896 * 1000000);*/


        Transaction txResponse = webClient
                .get()
                .uri(Constants.BLOCKFROST_TX_ENDPOINT.replaceFirst("\\{hash\\}", hash))
                .retrieve().bodyToMono(Transaction.class).block();

        assertEquals(hash, txResponse.getHash());
        assertNotNull(txResponse.getBlock());
        assertNotEquals("", txResponse.getBlock());


       /*TransactionUtxoInputs txIn = ListIterate.detect(txResponse.getInputs(), e -> e.getAddress().equals(addrSender));
        TransactionOutputAmount lovelaceTxIn = ListIterate.detect(txIn.getAmount(), e -> e.getUnit().equals(LOVELACE));
        assertEquals(inputLovelace, lovelaceTxIn.getQuantity());

        TransactionUtxoOutputs txOut = ListIterate.detect(txResponse.getOutputs(), e -> e.getAddress().equals(addrRecipient));
        TransactionOutputAmount lovelaceTxOut = ListIterate.detect(txOut.getAmount(), e -> e.getUnit().equals(LOVELACE));
        assertEquals(outputLovelace, lovelaceTxOut.getQuantity());
        TransactionUtxoOutputs txOutChange = ListIterate.detect(txResponse.getOutputs(), e -> e.getAddress().equals(addrSender));
        TransactionOutputAmount lovelaceTxOutChange = ListIterate.detect(txOutChange.getAmount(), e -> e.getUnit().equals(LOVELACE));
        assertEquals(changeLovelace, lovelaceTxOutChange.getQuantity());*/

    }


}
