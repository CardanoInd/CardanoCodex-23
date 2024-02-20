# ADP Script

## 1. Project

ADP Script - Bulk NFTs/FTs Distribution

By Team Vignesh

## 2. Project's Description

Simplifying the airdrop process of NFTs/FTs to multiple wallets. With a CSV file that has addresses and tokens, By running this script, It will automatically combine and airdrop tokens to the respective addresses in individual transactions.

## 3. What problem you are trying to solve

Sending NFTs/FTs manually to multiple wallets can be frustrating. This script simplifies the process of airdrop by allowing you to provide the addresses and tokens in a CSV format, and it will start the airdrop automatically.

## 4. Tech Stack used while building the project

Blockfrost - Cardano API

Meshjs - Cardano SDK

## 5. Project Demo Photos, Videos

1. Configure your CSV to have columns "Address" and "Token".
    1. The "Address" column should contain the stakekeys.
    2. The "Token" column should have the unit ID of the assets.
2. Import CSV to the root folder and run.

```
deno run -A airdrop.ts
```

1. That’s it!

![Image](https://i.postimg.cc/5yNgZnF7/image.png)

## 6. If your project is deployed, then include the Live Project Link

## 7. PPT link

[PPT](/Vignesh/PPT.pdf)

## 8. Team Member Info

- *Vigneshwaran B*
- GitHub: vigneshmahaan
- Email: [rockyvignesh312@gmail.com](mailto:rockyvignesh312@gmail.com)
