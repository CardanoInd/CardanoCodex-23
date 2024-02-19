1. Project
    RESTful endpoints for Cardano Smart Contracts 

2. Description
   We improvise Plutus smart contracts to add an api layer that can provide information related to the contracts at runtime over REST apis.

   Supported endpoints in this project

<ul>

<li>:heavy_check_mark: Plutus pub key hash for a given bech32 address</li>
<li>:heavy_check_mark: Parameterized script address</li>
<li>:heavy_check_mark: CBOR encoded script generated with Plutus</li>
<li>:heavy_check_mark: Datum hash as encoded by Plutus</li>
<li>:heavy_check_mark: Serialized datum</li>
<li>:heavy_check_mark: Policy id / currency symbol for parametrised minting contracts</li>

</ul>


3. Problem we are trying to solve
    It isn't easy to bridge front end and back end for smart contract dapps. By providing a template for an API layer over the smart contracts, we guide teams to create a bridge to backend smart contract code accessible easily over http protocol. 

4. Tech Stack used while building the project
    Language: Haskell
    Tooling dependency: Cabal, Nix 

5. Project Demo Photos, Videos
    ### **Video walkthrough:**

    For a walkthrough on how to set up and use the tooling please watch this [video](https://youtu.be/6jOAhqwzyag).


6. Live Project Link
    This project's design can be emulated by teams designing their smart contracts. By modifying the parameters/function names, the api endpoints can be activated in other projects. The functionality could be extended too.  

7. PPT link

8. Team Info
    Team name: Lambdac
    Contact: Vinu Anil
    Email: vinuanilg@gmail.com

9. How to use

    Include the attribute server into your plutus project by either using this project as a template or by copy the files under `app` and `src` into your project. At runtime, you need to run the `endpoint-server` executable which exposes the RESTful endpoints. For your custom projects update the deifinitions to match your smart contract's validator and its parametrisation. Some example contracts (Escrow & TokenMinter) are  given in this project that shows end to end functionality.  

 
### Running the attribute server

1. Run `nix-shell` from plutus-apps (clone from https://github.com/lambdacc/plutus-apps) and switch back to this project dir (_/lambdac_)

2. Build the executable
```bash
cabal build endpoint-server  
```

3. Run the web server for script address generation

```bash
 ./start-endpoint-server.sh
```

### Available endpoints


**1. Get pkh of a bech32 address**

```bash
    curl -X 'GET' \
      'http://localhost:9033/pkh/addr1qxfp6e75mf0za3f5u5q4urhnq7f2gpml5rnfze894e67mwj7ue7m5n322j9a99jldr2hwmfg068ngjg7fh6g9wy60fcq9al7fz'

```
- Response:
```
    {"pkh":{"unPaymentPubKeyHash":{"getPubKeyHash":"921d67d4da5e2ec534e5015e0ef30792a4077fa0e69164e5ae75edba"}}}
```

**2. Get script address**
```bash
    curl -X 'POST' \
          'http://localhost:9033/script-address' \
          -H 'accept: application/json;charset=utf-8' \
          -H 'Content-Type: application/json;charset=utf-8' \
          -d '{
               "reqBuyer":"addr1q88pume7lvnfzjt237jax8kv2jj8mjy8vj9lprs40nvg0tpdhh6uds3um9clypqqe59uaklndh632truwujvkdun7pvsrvzj0r",
               "reqSeller":"addr1qy6k4hdzynj6yv70uuvg8rxmjlxyvt7mm4kadc6ma29pftzf08rll0kaljffd8jhaqx86jkusxd20v2ql222jae3k9dqa6rnuj",
               "reqAmount":'800000000',
               "reqFinaliseTime":'1650334630000',
               "reqEndTime":'1650384630000',
               "reqNetwork":"mainnet"
              }'
```

 - Response
```
    {"bech32Address":"addr1w80kqf0r3phtp7at2l0f7wfqawdhudmtdqenqkyrqgld0xsyjrt05"}
```

**3. Get script CBOR**
```bash
    curl -X 'POST' \
          'http://localhost:9033/script-cbor' \
          -H 'accept: application/json;charset=utf-8' \
          -H 'Content-Type: application/json;charset=utf-8' \
          -d '{
               "reqBuyer":"addr1q88pume7lvnfzjt237jax8kv2jj8mjy8vj9lprs40nvg0tpdhh6uds3um9clypqqe59uaklndh632truwujvkdun7pvsrvzj0r",
               "reqSeller":"addr1qy6k4hdzynj6yv70uuvg8rxmjlxyvt7mm4kadc6ma29pftzf08rll0kaljffd8jhaqx86jkusxd20v2ql222jae3k9dqa6rnuj",
               "reqAmount":'800000000',
               "reqFinaliseTime":'1650334630000',
               "reqEndTime":'1650384630000',
               "reqNetwork":"mainnet"
              }'
```

 - Response
```
    {"type":"PlutusScriptV1","cborHex":"590a0a590a070100003323232323232323232323232323233223232323222322322323253353330083333573466e1cd55cea803240004642460020046eb8d5d09aab9e50072326353357380380340320306666ae68cdc3a802240084244400446666ae68cdc3a802a40044244400646666ae68cdc3a8032400042444002464c6a66ae7007807006c068064060cccd5cd19b8735573aa004900011991091980080180119191919191919191919191999ab9a3370e6aae754029200023333333333222222222212333333333300100b00a009008007006005004003002335016232323333573466e1cd55cea8012400046644246600200600460426ae854008c06cd5d09aba2500223263533573805805405205026aae7940044dd50009aba1500a33501601735742a012666aa032eb94060d5d0a804199aa80cbae501835742a00e66a02c0426ae854018cd4058cd54090089d69aba150053232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd409dd69aba150023028357426ae8940088c98d4cd5ce01801701681609aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a813bad35742a00460506ae84d5d1280111931a99ab9c03002e02d02c135573ca00226ea8004d5d09aba2500223263533573805805405205026aae7940044dd50009aba1500433501675c6ae85400ccd4058cd54091d710009aba15002301e357426ae8940088c98d4cd5ce01401301281209aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aab9e5001137540026ae854008c8c8c8cccd5cd19b875001480188c848888c010014c064d5d09aab9e500323333573466e1d40092004232122223002005301b357426aae7940108cccd5cd19b875003480088c848888c004014c05cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931a99ab9c02302102001f01e01d01c135573aa00226ea8004d5d09aba250022326353357380380340320302032264c6a66ae712410350543500019018135573ca00226ea80044d55ce9baa001137540022464460046eb0004c8004d5405088cccd55cf80092804919a80418021aba100230033574400402646464646666ae68cdc39aab9d5003480008ccc88848ccc00401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500e014357426ae8940088c98d4cd5ce00d00c00b80b09aab9e5001137540026ae85400cccd5401dd728031aba1500233500a75c6ae84d5d1280111931a99ab9c016014013012135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5404888c8cccd55cf80112804119a80399aa80a18031aab9d5002300535573ca00460086ae8800c0484d5d080088910010910911980080200189119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6a66ae7004804003c0380344d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6a66ae7003c03403002c4d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263533573801a01601401226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263533573802001c01a01801601426aae7540044dd50009191999ab9a3370ea0029001109100111999ab9a3370ea0049000109100091931a99ab9c00c00a009008007135573a6ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263533573802802402202001e01c01a01801601426aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931a99ab9c00d00b00a009008135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98d4cd5ce00500400380300289aab9d5001137540022244464646666ae68cdc39aab9d5002480008cd54024c018d5d0a80118029aba135744a004464c6a66ae7002802001c0184d55cf280089baa00149848004448848cc00400c0092410350543100112323001001223300330020020013323322323322323232323322322222323500522222533553355333500813300b500600513300b5006004132635335738920110496e76616c69642072656465656d6572001200149840344cd5ce2481174d697373696e672076616c6964207369676e61747572650000c153353500925333500915335333573466e3c0040180380344ccd54c03c48004c8cd404488ccd400c88008008004d40048800448cc004894cd40084004404003c8004cd54c0444800494cd4d4004888004840404038ccd54c04848004894cd54cd4d4d400888800c88cd4008940608d406400484044403c4cd40600080044005405ccd54c054480048d400488004d5401c888888888802840344038403840344cd5ce2490e496e76616c696420696e707574730000c100c1350012200222350022222222222533533355300f12001335010225335002210031001501325335333573466e3c03000403c0384d40540045405000c8403c4034488008488004c8004d5402488448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448848cc00400c008c8004d5401c8844894cd400454028884cd402cc010008cd54c01848004010004c8004d5401888448894cd40044008884cc014008ccd54c01c4800401401000448488c00800c4488004c8004d5400c8844894cd400454018884cd401cc010008cd54c018480040100044488008488488cc00401000c448c8c00400488cc00cc008008004cd4488ccccc00922011cce1e6f3efb2691496a8fa5d31ecc54a47dc887648bf08e157cd887ac0048811c356adda224e5a233cfe718838cdb97cc462fdbdd6dd6e35bea8a14ac004820283e3e81520e081e0f98760483830edea621808888848ccccc00401801401000c0088005","description":""}
```


**4. Get the datum hash for your contract**

```bash
    curl -X 'GET' \
      'http://localhost:9033/datum-hash/addr1qyyp95xmzg8m29m3rzy9ss7h565vd40eacgmzr8mnkf7734mte34znrh2muldmf5ty9npsqeqv7cn07lxly6jqlv6vqq3zt4u4'

```
- Response:
```
    {"datumHash":"3d940f8546c90218f43bbc394a93d320c26279f13301116163a440a871d6650b"}
```

**5. Get serialised datum for your contract**

```bash
    curl -X 'GET' \
      'http://localhost:9033/datum/addr1qyyp95xmzg8m29m3rzy9ss7h565vd40eacgmzr8mnkf7734mte34znrh2muldmf5ty9npsqeqv7cn07lxly6jqlv6vqq3zt4u4'

```
- Response:
```
    {"datum":{"fields":[{"bytes":"0812d0db120fb5177118885843d7a6a8c6d5f9ee11b10cfb9d93ef46"}],"constructor":0}}
```


**6. Get policy id for a parametrised token minting contract**
```bash
    curl -X 'POST' \
          'http://localhost:9033/policy-id' \
          -H 'accept: application/json;charset=utf-8' \
          -H 'Content-Type: application/json;charset=utf-8' \
          -d '{"issuerAddress":"addr1q88pume7lvnfzjt237jax8kv2jj8mjy8vj9lprs40nvg0tpdhh6uds3um9clypqqe59uaklndh632truwujvkdun7pvsrvzj0r","tokenName":"CardanoUniverse"}'
```

- Response:
```
    {"policyId":"390f10ca532acdc02cb20604d97f65195ffa2252c5b86f982b73bc55"}
```
