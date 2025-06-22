type t = Testnet | Mainnet | Other_network of string

let t_DEPRECATED =
  match Sys.getenv_opt "ZEKO_SIGNATURE_KIND" with
  | Some "testnet" ->
      Testnet
  | Some "mainnet" ->
      Mainnet
  | Some x ->
      Other_network x
  | None ->
      Testnet
