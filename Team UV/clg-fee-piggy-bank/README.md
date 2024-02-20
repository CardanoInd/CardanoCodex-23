
## 1. Project
    
**College Piggy Bank** : A Piggy Bank Project for promoting good financial planning towards paying their college fees and dollar cost averaging into high yield investments.

By Team UV

## 2. Project's Description

A fully decentralized Cardano-based piggy bank designed for college students to cultivate healthy savings and investment habits, potentially aiding in accumulating funds for their college fees and other expenses.


## 3. What problem you are trying to solve
The project addresses the financial challenges facing college students by offering a decentralized Cardano-based piggy bank. It aims to tackle issues such as financial illiteracy, stress from tuition fees and expenses, limited access to investments, and a lack of savings discipline. By providing an accessible platform for savings and investments, the project seeks to empower students with essential financial skills, encouraging responsible financial management and facilitating the accumulation of funds for college expenses and other financial goals.


## 4. Tech Stack used while building the project
- Smart Contract: Opshin & Aiken
- Cardano Piggy Bank Wallet: Cardano Native Script
- Blockfrost: Submit API
- Frontend: ReactJS
- Offchain: MeshJS and Lucid

## 5. Project Demo Photos, Videos

### Deposit in the piggy-bank

https://preprod.cardanoscan.io/transaction/f3fb776e36f8fe87f1c0748411f8f9dc19c70d505cd71e5e02a96a52d1304fac?tab=utxo


### Fees Payment Date
![enter image description here](https://i.postimg.cc/fy88HH1g/date.jpg)

### Student Wallet
![enter image description here](https://i.postimg.cc/nhP6VLRw/Wallet.png)

### Create Piggy Bank Wallet

Generate a wallet with Cardano Native Script by inputting date and time which automatically convert to slot and also add signer address that can access the fund after mentioned date


### Run Contract

Once you have entered the poetry shell, you can start interacting with the contract through the prepared scripts.

First, we have to build the vesting contract and generate two key pairs, one for the owner of funds and one for the intended beneficiary.

python3 scripts/build.py
python3 scripts/create_key_pair.py owner
python3 scripts/create_key_pair.py beneficiary
Make sure that the owner is loaded up with some testnet ada before proceeding, by using the testnet faucet. You can find the address of the owner key by running this command

###  cat keys/owner.addr
After requesting ada for the owner, send some ada to the beneficiary. The receiver address needs a small amount of ada in order to provide it as collateral when unlocking the funds later.

###  python3 src/off_chain/distribute.py owner beneficiary 
Then you can place a vested amount of ada at the contract. If you just requested funds for the owner address, you might need to wait a few minutes or the script will display an error that funds are missing.

###  python3 src/off_chain/make_vest.py owner beneficiary 
By default the deadline is 0 seconds after the creation of the vesting, so you can directly proceed and unlock the vested amount with the beneficiary!

### python3 src/off_chain/collect_vest.py beneficiary
That's it! You successfully compiled a Smart Contract on cardano and interacted with it through off-chain tooling. Feel free to dive into the provided scripts and start customizing them for your needs.


## 6. If your project is deployed, then include the Live Project Link
-  NA

## 7. PPT link 
- https://docs.google.com/file/d/1_cCdG08bsD3hZIj7Oy1Hkuqj9j4QkPJ5/edit?filetype=mspresentation

## 8. Team Member Info
- **Sai Yuvan**
   - GitHub: Sai27yuvan
   - Email: [saiyuvan007@gmail.com](mailto:saiyuvan007@gmail.com)