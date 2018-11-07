Robonomics network tools 
========================

[![Build Status](https://travis-ci.org/airalab/robonomics-tools.svg?branch=master)](https://travis-ci.org/airalab/robonomics-tools)

Build
-----

Using [Stackage](https://docs.haskellstack.org/en/stable/README/) with [Nix](https://nixos.org/nix/) integration:

    git clone https://github.com/airalab/robonomics-tools && cd robonomics-tools
    stack build --nix

* [Ubuntu install guide](https://github.com/airalab/robonomics-tools/tree/master/docs/ubuntu_install_guide.md)

Usage
-----

* Robot liability smart contract operations

```
Usage: liability COMMAND
  Robonomics liability smart contract ops

Available options:
  -h,--help                Show this help text

Available commands:
  read                     Read liability smart contract
  list                     List liability smart contracts
```

* The XRTd: Robonomics network provider

```
Usage: xrtd [--web3 URI] --private KEY [--chain CHAIN_ID] [--ipfs MULTIADDR]
            [--lighthouse ENS] [--ens ADDRESS]
  XRTd :: Robonomics network provider

Available options:
  --web3 URI               Ethereum node endpoint [DEFAULT: Infura mainnet]
  --private KEY            Hex encoded private key
  --chain CHAIN_ID         Ethereum chain [foundation, ropsten, kovan, rikenby]
  --ipfs MULTIADDR         IPFS node endpoint [DEFAULT: localhost]
  --lighthouse ENS         Robonomics lighthouse name
  --ens ADDRESS            ENS registry contract
  -h,--help                Show this help text
```

Examples
--------

* `xrtd` on Ethereum mainnet 

```
    stack exec xrtd -- --private "40c0901dbcb03e43df4e3ec432f15168ec390f3d861227d12e115c06c9d8f1ca"
```

> If you want to give more quota for your provider, please visit [Lighthouse manager Dapp]().

* `xrtd` on cliquebait testnet

```
    stack exec xrtd -- --private "40c0901dbcb03e43df4e3ec432f15168ec390f3d861227d12e115c06c9d8f1ca" --web3 http://localhost:8545 --ens "0x3c6d5ee6c4d9067a3c58906f3689534aa30de02b" --chain 420123
```

---

**Have fun!**
