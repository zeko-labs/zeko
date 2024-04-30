import { useMutation, useQueryClient } from "@tanstack/react-query";
import { LOCAL_STORAGE_KEYS } from "../utils/localStorage";
import { Transaction } from "../utils/types";
import QueryKey from "../utils/queryKeys";

export const editStoredTransaction = async (tx: Partial<Transaction>) => {
  const item = localStorage.getItem(LOCAL_STORAGE_KEYS.TRANSACTIONS);
  const txArray = JSON.parse(item ?? "[]") as Transaction[];
  const index = txArray.findIndex((_tx) => _tx.id === tx.id);
  if (index === -1) {
    return;
  }
  const prevTx = txArray[index];
  const newTx = { ...prevTx, ...tx };
  txArray.splice(index, 1, newTx);
  localStorage.setItem(
    LOCAL_STORAGE_KEYS.TRANSACTIONS,
    JSON.stringify(txArray)
  );
};

export default function useEditStoredTransaction() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: editStoredTransaction,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: [QueryKey.TRANSACTIONS] });
    },
  });
}
