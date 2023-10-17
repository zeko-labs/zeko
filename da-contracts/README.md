# Zeko data availability contracts

## Installation

```bash
npm install
npm run compile
```

To install geth with mina precompiles:

```bash
git clone git@github.com:dcSpark/go-ethereum.git
cd go-ethereum
git checkout release/1.10-mina
make geth
export PATH="$PWD/build/bin:$PATH"
```

## Deploy of data availability contract

```bash
npm run geth:start
npx hardhat run scripts/deploy.ts --network dev
```

## Testing

```bash
npm run compile
npm run test
```
