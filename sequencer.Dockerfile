FROM node:16 AS x-builder

COPY ./o1js /o1js

COPY ./sequencer/src /sequencer/src
COPY ./sequencer/package.json /sequencer/
COPY ./sequencer/tsconfig.json /sequencer/
COPY ./sequencer/schema.graphql /sequencer/

COPY ./da-contracts/contracts /da-contracts/contracts
COPY ./da-contracts/package*.json /da-contracts/
COPY ./da-contracts/tsconfig.json /da-contracts/
COPY ./da-contracts/hardhat.config.ts /da-contracts/

WORKDIR /da-contracts

RUN npm ci && npm run compile

WORKDIR /sequencer

RUN npm install /o1js
RUN npm run build

ENTRYPOINT ["node", "build/index.js"]
