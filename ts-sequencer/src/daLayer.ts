import { ethers } from 'ethers';
import { abi as DALayerAbi } from './artifacts/DataAvailability.json';
import { DataAvailability } from './typechain-types';

export const provider = new ethers.providers.WebSocketProvider(
  'ws://localhost:8546'
);

export const wallet = new ethers.Wallet(
  '0x35f9400884bdd60fdd1a769ebf39fa1c5b148072e68a5b2c8bc9ac2227c192b2',
  provider
);

export const daLayerContract = new ethers.Contract(
  '0x0A3825CD4b413141e89249620e1Ad98cf2Ac6B24',
  DALayerAbi,
  wallet
) as DataAvailability;
