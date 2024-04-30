export enum WrappingDirection {
  WRAP = "WRAP",
  UNWRAP = "UNWRAP",
}

export enum TransactionStatus {
  PENDING = "PENDING",
  IN_PROGRESS = "IN PROGRESS",
  COMPLETE = "COMPLETE",
  ERROR = "ERROR",
}

export type Transaction = {
  id: string;
  status: TransactionStatus;
  amount: string;
  direction: WrappingDirection;
};
