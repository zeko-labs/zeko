open Core_kernel
open Async
open Amqp_client_async

let jobs_queue = "sequencer.jobs"

module Master = struct
  type t =
    { connection : Amqp.Connection.t
    ; client : Amqp.Rpc.Client.t
    ; mutable counter : int
    }

  let start host_and_port =
    let%bind connection =
      Amqp.Connection.connect
        ~id:("sequencer.connection." ^ (Uuid_unix.create () |> Uuid.to_string))
        ~port:(Host_and_port.port host_and_port)
        (Host_and_port.host host_and_port)
    in
    let%map client =
      Rpc.Client.init
        ~id:("sequencer.client." ^ (Uuid_unix.create () |> Uuid.to_string))
        connection
    in
    { connection; client; counter = 0 }

  (** Works only with single producer *)
  let get_queue_size t = t.counter

  let send' ~priority t payload =
    t.counter <- t.counter + 1 ;
    match%map
      Rpc.Client.call
        ~ttl:(60 * 60 * 1_000)
        t.client Exchange.default ~routing_key:jobs_queue ~headers:[]
        (Message.make ~priority payload)
    with
    | Some (_h, s) ->
        t.counter <- t.counter - 1 ;
        s
    | None ->
        failwith "Error sending job to the message queue"

  let send = send' ~priority:0

  let send_with_priority = send' ~priority:1
end

module Worker = struct
  let start host_and_port handler =
    let%bind connection =
      Amqp.Connection.connect ~id:"worker.connection"
        ~port:(Host_and_port.port host_and_port)
        (Host_and_port.host host_and_port)
    in
    let%bind channel =
      Amqp.Connection.open_channel ~id:"worker.channel" Channel.no_confirm
        connection
    in
    let%bind queue =
      Amqp.Queue.declare channel
        ~arguments:[ Rpc.Server.queue_argument; Amqp.Queue.maximum_priority 5 ]
        jobs_queue
    in
    let%map server =
      Rpc.Server.start channel queue (fun (_h, s) ->
          let%map result = handler s in
          Message.make result )
    in
    server
end
