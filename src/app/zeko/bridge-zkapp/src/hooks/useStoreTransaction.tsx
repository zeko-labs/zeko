import { useMutation, useQueryClient } from "@tanstack/react-query";
import { LOCAL_STORAGE_KEYS } from "../utils/localStorage";
import QueryKey from "../utils/queryKeys";
import { Transaction } from "../utils/types";

const storeTransaction = async (tx: Transaction) => {
  const item = localStorage.getItem(LOCAL_STORAGE_KEYS.TRANSACTIONS);
  const txArray = JSON.parse(item ?? "[]") as Transaction[];
  txArray.push(tx);
  localStorage.setItem(
    LOCAL_STORAGE_KEYS.TRANSACTIONS,
    JSON.stringify(txArray)
  );
};

export default function useStoreTransaction() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: storeTransaction,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: [QueryKey.TRANSACTIONS] });
    },
  });
}
