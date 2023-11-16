# Zeko sequencer

Experimental version of the mina rollup sequencer in typescript

## Installation

```bash
npm install
```

## Usage

First [run the DA network](../da-layer/contracts/README.md#deploy-of-data-availability-contract).

Then run the sequencer:

```bash
DA_LAYER_CONTRACT_ADDRESS=<address output from deployment> npm run dev
```
