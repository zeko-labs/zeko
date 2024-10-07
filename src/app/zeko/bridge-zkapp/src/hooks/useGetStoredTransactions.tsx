import { useQuery } from "@tanstack/react-query";
import QueryKey from "../utils/queryKeys";
import { Transaction } from "../utils/types";
import { LOCAL_STORAGE_KEYS } from "../utils/localStorage";

export const getTransactions = () => {
  return JSON.parse(
    localStorage.getItem(LOCAL_STORAGE_KEYS.TRANSACTIONS) ?? "[]"
  ) as Transaction[];
};

export default function useStoredTransactions() {
  return useQuery({
    queryKey: [QueryKey.TRANSACTIONS],
    queryFn: getTransactions,
  });
}
