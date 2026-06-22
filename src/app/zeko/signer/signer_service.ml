open Core_kernel
open Async
open Mina_base
open Signature_lib
module Field = Snark_params.Tick.Field

let signature_kind_to_string = function
  | Mina_signature_kind.Mainnet ->
      "mainnet"
  | Testnet ->
      "testnet"
  | Other_network network_id ->
      network_id

let signature_kind_of_string = function
  | "mainnet" ->
      Mina_signature_kind.Mainnet
  | "testnet" ->
      Mina_signature_kind.Testnet
  | network_id ->
      Mina_signature_kind.Other_network network_id

module Policy = struct
  module Zkapp = struct
    type t =
      { max_fee : Currency.Fee.t option
      ; max_balance_change : Currency.Amount.t option
      }
    [@@deriving sexp]
  end

  type t = { allow_field_signing : bool; zkapp : Zkapp.t option }
  [@@deriving sexp]
end

module Rpc = struct
  module Auth = struct
    type t = { auth_token : string } [@@deriving bin_io]
  end

  module Get_public_key = struct
    module V1 = struct
      module Query = Auth

      let t : (Query.t, Public_key.Compressed.t) Async.Rpc.Rpc.t =
        Async.Rpc.Rpc.create ~name:"Signer_get_public_key" ~version:1
          ~bin_query:Query.bin_t
          ~bin_response:Public_key.Compressed.Stable.V1.bin_t
    end
  end

  module Sign_field = struct
    module V1 = struct
      module Query = struct
        type t =
          { auth_token : string; signature_kind : string; field : Field.t }
        [@@deriving bin_io]
      end

      module Response = struct
        type t = (Signature.Stable.V1.t, string) Result.t [@@deriving bin_io]
      end

      let t : (Query.t, Response.t) Async.Rpc.Rpc.t =
        Async.Rpc.Rpc.create ~name:"Signer_sign_field" ~version:1
          ~bin_query:Query.bin_t ~bin_response:Response.bin_t
    end
  end

  module Sign_zkapp_command = struct
    module V1 = struct
      module Query = struct
        type t =
          { auth_token : string
          ; signature_kind : string
          ; command : Zkapp_command.Stable.V1.t
          }
        [@@deriving bin_io]
      end

      module Response = struct
        type t = (Zkapp_command.Stable.V1.t, string) Result.t
        [@@deriving bin_io]
      end

      let t : (Query.t, Response.t) Async.Rpc.Rpc.t =
        Async.Rpc.Rpc.create ~name:"Signer_sign_zkapp_command" ~version:1
          ~bin_query:Query.bin_t ~bin_response:Response.bin_t
    end
  end

  module Sign_fee_payer = struct
    module V1 = struct
      module Query = struct
        type t =
          { auth_token : string
          ; signature_kind : string
          ; command : Zkapp_command.Stable.V1.t
          }
        [@@deriving bin_io]
      end

      module Response = struct
        type t = (Zkapp_command.Stable.V1.t, string) Result.t
        [@@deriving bin_io]
      end

      let t : (Query.t, Response.t) Async.Rpc.Rpc.t =
        Async.Rpc.Rpc.create ~name:"Signer_sign_fee_payer" ~version:1
          ~bin_query:Query.bin_t ~bin_response:Response.bin_t
    end
  end
end

module Tls = struct
  module Ssl = Async_ssl.Ssl

  module Client_config = struct
    type t = { ca_file : string; expected_host : string }

    let of_env location =
      Option.map (Sys.getenv "ZEKO_SIGNER_TLS_CA_FILE") ~f:(fun ca_file ->
          let expected_host =
            Option.value
              (Sys.getenv "ZEKO_SIGNER_TLS_HOSTNAME")
              ~default:(Host_and_port.host location)
          in
          { ca_file; expected_host } )
  end

  module Server_config = struct
    type t = { cert_file : string; key_file : string }
  end

  let connect_pipes reader writer =
    let net_to_ssl_r, net_to_ssl_w = Pipe.create () in
    let ssl_to_net_r, ssl_to_net_w = Pipe.create () in
    don't_wait_for
      ( Pipe.transfer (Reader.pipe reader) net_to_ssl_w ~f:Fn.id
      >>| fun () -> Pipe.close net_to_ssl_w ) ;
    don't_wait_for
      ( Pipe.iter ssl_to_net_r ~f:(fun data ->
            Writer.write writer data ; Writer.flushed writer )
      >>| fun () -> don't_wait_for (Writer.close writer) ) ;
    (net_to_ssl_r, ssl_to_net_w)

  let app_streams ~name app_to_ssl_w ssl_to_app_r =
    let%bind reader =
      Reader.of_pipe (Info.of_string (name ^ "-reader")) ssl_to_app_r
    in
    let%map writer, _ =
      Writer.of_pipe (Info.of_string (name ^ "-writer")) app_to_ssl_w
    in
    (reader, writer)

  let certificate_matches_host cert expected_host =
    let san_matches =
      Ssl.Certificate.subject_alt_names cert
      |> List.exists ~f:(String.equal expected_host)
    in
    let cn_matches =
      Ssl.Certificate.subject cert
      |> List.exists ~f:(fun (name, value) ->
             String.equal name "CN" && String.equal value expected_host )
    in
    san_matches || cn_matches

  let validate_server_certificate connection expected_host =
    match Ssl.Connection.peer_certificate connection with
    | None ->
        Or_error.error_string "TLS server did not provide a certificate"
    | Some (Error err) ->
        Error err
    | Some (Ok cert) ->
        if certificate_matches_host cert expected_host then Ok ()
        else
          Or_error.errorf
            "TLS server certificate does not match expected signer host %s"
            expected_host

  let client_streams ~config ~location reader writer =
    let net_to_ssl_r, ssl_to_net_w = connect_pipes reader writer in
    let app_to_ssl_r, app_to_ssl_w = Pipe.create () in
    let ssl_to_app_r, ssl_to_app_w = Pipe.create () in
    let%bind connection =
      Ssl.client ~name:"zeko-signer-client"
        ~hostname:config.Client_config.expected_host ~ca_file:config.ca_file
        ~app_to_ssl:app_to_ssl_r ~ssl_to_app:ssl_to_app_w
        ~net_to_ssl:net_to_ssl_r ~ssl_to_net:ssl_to_net_w ()
      >>| Or_error.ok_exn
    in
    validate_server_certificate connection config.expected_host
    |> Or_error.ok_exn ;
    app_streams
      ~name:(sprintf "zeko-signer-client-%s" (Host_and_port.to_string location))
      app_to_ssl_w ssl_to_app_r

  let server_streams ~config reader writer =
    let net_to_ssl_r, ssl_to_net_w = connect_pipes reader writer in
    let app_to_ssl_r, app_to_ssl_w = Pipe.create () in
    let ssl_to_app_r, ssl_to_app_w = Pipe.create () in
    let%bind _connection =
      Ssl.server ~name:"zeko-signer-server"
        ~crt_file:config.Server_config.cert_file ~key_file:config.key_file
        ~app_to_ssl:app_to_ssl_r ~ssl_to_app:ssl_to_app_w
        ~net_to_ssl:net_to_ssl_r ~ssl_to_net:ssl_to_net_w ()
      >>| Or_error.ok_exn
    in
    app_streams ~name:"zeko-signer-server" app_to_ssl_w ssl_to_app_r
end

module Command_signing = struct
  let full_commitment ~signature_kind (command : Zkapp_command.t) =
    Zkapp_command.Transaction_commitment.create_complete
      (Zkapp_command.commitment command)
      ~memo_hash:(Signed_command_memo.hash command.memo)
      ~fee_payer_hash:
        (Zkapp_command.Digest.Account_update.create ~signature_kind
           (Account_update.of_fee_payer command.fee_payer) )

  let validate_policy ~(public_key : Public_key.Compressed.t)
      (policy : Policy.Zkapp.t) (command : Zkapp_command.t) =
    let open Result.Let_syntax in
    let%bind () =
      match policy.max_fee with
      | None ->
          Ok ()
      | Some max_fee ->
          if
            Public_key.Compressed.equal public_key
              command.fee_payer.body.public_key
            && Currency.Fee.(command.fee_payer.body.fee > max_fee)
          then
            Or_error.errorf "Fee %s exceeds configured maximum %s"
              (Currency.Fee.to_string command.fee_payer.body.fee)
              (Currency.Fee.to_string max_fee)
          else Ok ()
    in
    match policy.max_balance_change with
    | None ->
        Ok ()
    | Some max_balance_change ->
        Zkapp_command.account_updates_list command
        |> List.filter ~f:(fun account_update ->
               Public_key.Compressed.equal public_key
                 account_update.body.public_key
               &&
               match account_update.body.authorization_kind with
               | Signature ->
                   true
               | _ ->
                   false )
        |> List.fold_result ~init:() ~f:(fun () account_update ->
               let magnitude =
                 Currency.Amount.Signed.magnitude
                   account_update.body.balance_change
               in
               if Currency.Amount.(magnitude > max_balance_change) then
                 Or_error.errorf
                   "Balance change %s exceeds configured maximum %s for %s"
                   (Currency.Amount.to_string magnitude)
                   (Currency.Amount.to_string max_balance_change)
                   (Public_key.Compressed.to_base58_check public_key)
               else Ok () )

  let sign_local ~signature_kind ~(keypair : Keypair.t) ?policy
      (command : Zkapp_command.t) =
    let public_key = Public_key.compress keypair.public_key in
    let open Result.Let_syntax in
    let%bind () =
      match policy with
      | None ->
          Ok ()
      | Some policy ->
          validate_policy ~public_key policy command
    in
    let full_commitment = full_commitment ~signature_kind command in
    let sign_raw msg =
      Signature_lib.Schnorr.Chunked.sign ~signature_kind keypair.private_key
        (Random_oracle.Input.Chunked.field msg)
    in
    let signed_any = ref false in
    let rec sign_tree
        (tree :
          ( Account_update.t
          , Zkapp_command.Digest.Account_update.t
          , Zkapp_command.Digest.Forest.t )
          Zkapp_command.Call_forest.Tree.t ) =
      let account_update =
        match tree.account_update.body.authorization_kind with
        | Signature
          when Public_key.Compressed.equal public_key
                 tree.account_update.body.public_key ->
            if not tree.account_update.body.use_full_commitment then
              failwith "Signer can only sign full-commitment account updates" ;
            signed_any := true ;
            { tree.account_update with
              authorization = Control.Poly.Signature (sign_raw full_commitment)
            }
        | _ ->
            tree.account_update
      in
      { tree with account_update; calls = sign_forest tree.calls }
    and sign_forest forest =
      List.map forest ~f:(fun tree -> { tree with elt = sign_tree tree.elt })
    in
    let fee_payer =
      if
        Public_key.Compressed.equal public_key command.fee_payer.body.public_key
      then (
        signed_any := true ;
        { command.fee_payer with authorization = sign_raw full_commitment } )
      else command.fee_payer
    in
    let command =
      { command with
        fee_payer
      ; account_updates = sign_forest command.account_updates
      }
    in
    if !signed_any then Ok command
    else
      Or_error.errorf
        "Signer key %s does not authorize any part of the zkApp command"
        (Public_key.Compressed.to_base58_check public_key)

  let sign_fee_payer_local ~signature_kind ~(keypair : Keypair.t) ?policy
      (command : Zkapp_command.t) =
    let public_key = Public_key.compress keypair.public_key in
    let open Result.Let_syntax in
    let%bind () =
      match policy with
      | None ->
          Ok ()
      | Some (policy : Policy.Zkapp.t) -> (
          match policy.max_fee with
          | None ->
              Ok ()
          | Some max_fee ->
              if
                Public_key.Compressed.equal public_key
                  command.fee_payer.body.public_key
                && Currency.Fee.(command.fee_payer.body.fee > max_fee)
              then
                Or_error.errorf "Fee %s exceeds configured maximum %s"
                  (Currency.Fee.to_string command.fee_payer.body.fee)
                  (Currency.Fee.to_string max_fee)
              else Ok () )
    in
    if
      not
        (Public_key.Compressed.equal public_key
           command.fee_payer.body.public_key )
    then
      Or_error.errorf "Signer key %s does not match the fee payer public key %s"
        (Public_key.Compressed.to_base58_check public_key)
        (Public_key.Compressed.to_base58_check command.fee_payer.body.public_key)
    else
      let full_commitment = full_commitment ~signature_kind command in
      let authorization =
        Signature_lib.Schnorr.Chunked.sign ~signature_kind keypair.private_key
          (Random_oracle.Input.Chunked.field full_commitment)
      in
      Ok { command with fee_payer = { command.fee_payer with authorization } }
end

module Client = struct
  type t =
    { logger : Logger.t
    ; location : Host_and_port.t
    ; auth_token : string
    ; tls_config : Tls.Client_config.t option
    ; public_key : Public_key.Compressed.t
    }

  let validate_auth_token auth_token =
    if String.is_empty auth_token then
      failwith "ZEKO_SIGNER_AUTH_TOKEN must not be empty"
    else auth_token

  let auth_token_from_env () =
    Sys.getenv_exn "ZEKO_SIGNER_AUTH_TOKEN" |> validate_auth_token

  let dispatch_tls ~logger:_ tls_config location rpc data =
    Deferred.Or_error.try_with_join ~here:[%here] (fun () ->
        Tcp.with_connection (Tcp.Where_to_connect.of_host_and_port location)
          ~timeout:(Time.Span.of_sec 1.) (fun _ reader writer ->
            let%bind reader, writer =
              Tls.client_streams ~config:tls_config ~location reader writer
            in
            match%bind
              Async.Rpc.Connection.create
                ~handshake_timeout:
                  (Time.Span.of_sec
                     Node_config_unconfigurable_constants
                     .rpc_handshake_timeout_sec )
                ~heartbeat_config:
                  (Async.Rpc.Connection.Heartbeat_config.create
                     ~timeout:
                       (Time_ns.Span.of_sec
                          Node_config_unconfigurable_constants
                          .rpc_heartbeat_timeout_sec )
                     ~send_every:
                       (Time_ns.Span.of_sec
                          Node_config_unconfigurable_constants
                          .rpc_heartbeat_send_every_sec )
                     () )
                reader writer
                ~connection_state:(fun _ -> ())
            with
            | Error exn ->
                return
                  (Or_error.errorf
                     !"Error connecting to the signer on \
                       %{sexp:Host_and_port.t} using TLS RPC %s: %s"
                     location (Async.Rpc.Rpc.name rpc) (Exn.to_string exn) )
            | Ok conn ->
                let%map result = Async.Rpc.Rpc.dispatch rpc conn data in
                don't_wait_for (Async.Rpc.Connection.close conn) ;
                result ) )

  let dispatch ?(max_tries = 5) ?(timeout = 5.) ~logger location ?tls_config rpc
      data =
    let rec go tries_left errs =
      if Int.( <= ) tries_left 0 then
        let e = Error.of_list (List.rev errs) in
        return
          (Error
             (Error.tag_arg e "Could not send query to signer" location
                Host_and_port.sexp_of_t ) )
      else
        match%bind
          match tls_config with
          | None ->
              Daemon_rpcs.Client.dispatch rpc data location
          | Some tls_config ->
              dispatch_tls ~logger tls_config location rpc data
        with
        | Ok result ->
            return (Ok result)
        | Error e ->
            let%bind () = after (Time.Span.of_sec timeout) in
            go (tries_left - 1) (e :: errs)
    in
    go max_tries []

  let create ~logger ~(location : Host_and_port.t) =
    let auth_token = auth_token_from_env () in
    let tls_config = Tls.Client_config.of_env location in
    let%map public_key =
      dispatch ~max_tries:1 ~logger location ?tls_config Rpc.Get_public_key.V1.t
        Rpc.Get_public_key.V1.Query.{ auth_token }
      >>| Or_error.ok_exn
    in
    { logger; location; auth_token; tls_config; public_key }

  let public_key t = t.public_key

  let sign_field ~signature_kind t field =
    dispatch ~logger:t.logger t.location ?tls_config:t.tls_config
      Rpc.Sign_field.V1.t
      Rpc.Sign_field.V1.Query.
        { auth_token = t.auth_token
        ; signature_kind = signature_kind_to_string signature_kind
        ; field
        }
    >>| function
    | Error err ->
        Error err
    | Ok (Ok signature) ->
        Ok signature
    | Ok (Error err) ->
        Error (Error.of_string err)

  let sign_zkapp_command ~signature_kind t command =
    dispatch ~logger:t.logger t.location ?tls_config:t.tls_config
      Rpc.Sign_zkapp_command.V1.t
      Rpc.Sign_zkapp_command.V1.Query.
        { auth_token = t.auth_token
        ; signature_kind = signature_kind_to_string signature_kind
        ; command
        }
    >>| function
    | Error err ->
        Error err
    | Ok (Ok serialized) ->
        Ok serialized
    | Ok (Error err) ->
        Error (Error.of_string err)

  let sign_fee_payer ~signature_kind t command =
    dispatch ~logger:t.logger t.location ?tls_config:t.tls_config
      Rpc.Sign_fee_payer.V1.t
      Rpc.Sign_fee_payer.V1.Query.
        { auth_token = t.auth_token
        ; signature_kind = signature_kind_to_string signature_kind
        ; command
        }
    >>| function
    | Error err ->
        Error err
    | Ok (Ok serialized) ->
        Ok serialized
    | Ok (Error err) ->
        Error (Error.of_string err)
end

module Signer = struct
  type t = Local of Keypair.t | Remote of Client.t

  let of_keypair keypair = Local keypair

  let of_client client = Remote client

  let public_key = function
    | Local keypair ->
        Public_key.compress keypair.public_key
    | Remote client ->
        Client.public_key client

  let public_key_decompressed t = Public_key.decompress_exn (public_key t)

  let sign_field ~signature_kind t field =
    match t with
    | Local keypair ->
        return
          (Ok
             (Signature_lib.Schnorr.Chunked.sign ~signature_kind
                keypair.private_key
                (Random_oracle.Input.Chunked.field field) ) )
    | Remote client ->
        Client.sign_field ~signature_kind client field

  let sign_zkapp_command ~signature_kind ?policy t command =
    match t with
    | Local keypair ->
        let signed =
          Command_signing.sign_local ~signature_kind ~keypair ?policy
            (Zkapp_command.write_all_proofs_to_disk ~signature_kind
               ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
               command )
          |> Result.map ~f:Zkapp_command.read_all_proofs_from_disk
        in
        return signed
    | Remote client ->
        Client.sign_zkapp_command ~signature_kind client command

  let sign_fee_payer ~signature_kind ?policy t command =
    match t with
    | Local keypair ->
        let signed =
          Command_signing.sign_fee_payer_local ~signature_kind ~keypair ?policy
            (Zkapp_command.write_all_proofs_to_disk ~signature_kind
               ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
               command )
          |> Result.map ~f:Zkapp_command.read_all_proofs_from_disk
        in
        return signed
    | Remote client ->
        Client.sign_fee_payer ~signature_kind client command
end

module Server = struct
  type t =
    { keypair : Keypair.t
    ; public_key : Public_key.Compressed.t
    ; policy : Policy.t
    ; logger : Logger.t
    ; auth_token : string
    ; tls_config : Tls.Server_config.t option
    }

  let create ?tls_config ~logger ~policy ~auth_token
      ~(private_key : Private_key.t) =
    let auth_token = Client.validate_auth_token auth_token in
    let keypair = Keypair.of_private_key_exn private_key in
    { keypair
    ; public_key = Public_key.compress keypair.public_key
    ; policy
    ; logger
    ; auth_token
    ; tls_config
    }

  let constant_time_compare s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    if Int.(len1 <> len2) then false
    else
      let result = ref 0 in
      for i = 0 to len1 - 1 do
        result :=
          !result
          lor (Char.to_int (String.get s1 i) lxor Char.to_int (String.get s2 i))
      done ;
      Int.(!result = 0)

  let check_auth t auth_token =
    if constant_time_compare auth_token t.auth_token then Ok ()
    else Error "Unauthorized signer RPC request"

  let handle_authenticated t auth_token f =
    match check_auth t auth_token with
    | Ok () ->
        f ()
    | Error err ->
        return (Error err)

  let implementations t =
    Async.Rpc.Implementations.create_exn ~on_unknown_rpc:`Close_connection
      ~implementations:
        [ Async.Rpc.Rpc.implement Rpc.Get_public_key.V1.t
            (fun () { auth_token } ->
              match check_auth t auth_token with
              | Ok () ->
                  return t.public_key
              | Error _ ->
                  failwith "Unauthorized signer RPC request" )
        ; Async.Rpc.Rpc.implement Rpc.Sign_field.V1.t
            (fun () { auth_token; signature_kind; field } ->
              handle_authenticated t auth_token (fun () ->
                  let signature_kind =
                    signature_kind_of_string signature_kind
                  in
                  if t.policy.allow_field_signing then
                    Signer.sign_field ~signature_kind
                      (Signer.of_keypair t.keypair)
                      field
                    >>| Result.map_error ~f:Error.to_string_hum
                  else return (Error "Field signing is disabled") ) )
        ; Async.Rpc.Rpc.implement Rpc.Sign_zkapp_command.V1.t
            (fun () { auth_token; signature_kind; command } ->
              handle_authenticated t auth_token (fun () ->
                  let signature_kind =
                    signature_kind_of_string signature_kind
                  in
                  match t.policy.zkapp with
                  | None ->
                      return (Error "zkApp command signing is disabled")
                  | Some policy ->
                      Signer.sign_zkapp_command ~signature_kind ~policy
                        (Signer.of_keypair t.keypair)
                        command
                      >>| Result.map_error ~f:Error.to_string_hum ) )
        ; Async.Rpc.Rpc.implement Rpc.Sign_fee_payer.V1.t
            (fun () { auth_token; signature_kind; command } ->
              handle_authenticated t auth_token (fun () ->
                  let signature_kind =
                    signature_kind_of_string signature_kind
                  in
                  match t.policy.zkapp with
                  | None ->
                      return (Error "zkApp command signing is disabled")
                  | Some policy ->
                      Signer.sign_fee_payer ~signature_kind ~policy
                        (Signer.of_keypair t.keypair)
                        command
                      >>| Result.map_error ~f:Error.to_string_hum ) )
        ]

  let is_loopback_host = function
    | "localhost" | "127.0.0.1" | "::1" ->
        true
    | _ ->
        false

  let run ~host ~port ~allow_insecure_remote_binding t =
    if
      (not (is_loopback_host host))
      && Option.is_none t.tls_config
      && not allow_insecure_remote_binding
    then
      failwithf
        "Refusing to bind signer service to non-loopback host %s without TLS. \
         Configure --tls-cert-file and --tls-key-file, or use \
         --allow-insecure-remote-binding only on a trusted private network."
        host () ;
    let%bind bind_address =
      if String.equal host "localhost" then return Tcp.Bind_to_address.Localhost
      else
        Unix.Inet_addr.of_string_or_getbyname host
        >>| fun address -> Tcp.Bind_to_address.Address address
    in
    let where_to_listen =
      Tcp.Where_to_listen.bind_to bind_address (Tcp.Bind_to_port.On_port port)
    in
    Tcp.Server.create
      ~on_handler_error:
        (`Call
          (fun _net exn ->
            let logger = t.logger in
            [%log error] "Signer RPC handler exception: %s"
              (Exn.to_string_mach exn) ) )
      where_to_listen
      (fun _ reader writer ->
        let%bind reader, writer =
          match t.tls_config with
          | None ->
              return (reader, writer)
          | Some config ->
              Tls.server_streams ~config reader writer
        in
        Async.Rpc.Connection.server_with_close reader writer
          ~implementations:(implementations t)
          ~connection_state:(fun _ -> ())
          ~on_handshake_error:`Ignore )
    >>| ignore
end
