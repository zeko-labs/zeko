import { useMutation, useQueryClient } from "@tanstack/react-query";
import { LOCAL_STORAGE_KEYS } from "../utils/localStorage";
import QueryKey from "../utils/queryKeys";

export default function useClearStoredTransactions() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: async () => {
      localStorage.removeItem(LOCAL_STORAGE_KEYS.TRANSACTIONS);
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: [QueryKey.TRANSACTIONS] });
    },
  });
}
