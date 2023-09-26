cd ../da-contracts
npm run geth:start > /dev/null 2>/dev/null &
geth_pid=$!
da_layer_address=$(npx hardhat run scripts/deploy.ts --network dev)
cd ../sequencer

npm run build

DA_LAYER_WEBSOCKET_URL=ws://localhost:8546 \
DA_LAYER_PRIVATE_KEY=0x35f9400884bdd60fdd1a769ebf39fa1c5b148072e68a5b2c8bc9ac2227c192b2 \
DA_LAYER_CONTRACT_ADDRESS=$da_layer_address \
COMMITMENT_PERIOD=0 \
npx jest build/tests

kill $geth_pid