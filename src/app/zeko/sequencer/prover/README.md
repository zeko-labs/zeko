# Zeko prover

This is a provers distribution for zeko sequencer.
It consists of a server that accepts a witness of some predetermined snark over tcp connection and returns a result.
Client is a sequencer itself, which picks one of the available provers and sends a request to it.

## Server

Server is a standalone application that listens on a tcp port and accepts a witness of a snark.
The `Input` type is the json serialized sum type of all the available witnesses.
The `Output` type is the json serialized sum type with same variants as `Input`.
I haven't figured how to make them one type with the serialization. My attempt was to create it as a GADT, but it didn't work with yojson.

The server is a simple pipe that reads the input from the client, deserializes it, calls the corresponding prover function and serializes the output back to the client.

## Client

Client is a library which consits of a state of all the available provers and function that picks one of them and sends a request to the server.

```ocaml
module State = struct
  type lazy_connection =
    ( ([ `Active ], Socket.Address.Inet.t) Socket.t * Reader.t * Writer.t
    , Error.t )
    Result.t
    Deferred.t
    lazy_t

  type prover_state = [ `In_use | `Available ]

  type t =
    { provers :
        (lazy_connection ref * Tcp.Where_to_connect.inet * prover_state ref)
        list
    ; mutable next : int
    }

let send ?(proving_timeout = 10.) ?(wait_for_prover_timeout = 600.)
    ?(attempts = 5) (state : State.t) (input : Prover.Input.t) : Prover.Output.t Deferred.t
```

## CLI

Run the prover worker (RabbitMQ consumer):

```bash
dune exec ./cli.exe -- run-server \
  --mq-host <string> \
  [--fake-proving-time <float>]
```

Options:
- `--mq-host` (required): RabbitMQ host:port for the prover queue.
- `--fake-proving-time` (optional): simulate proving time in seconds.

Send function picks one of the available provers, sends a request to it and returns the result.
It does by rotating provers list by `next` index and picking the first `Available` prover.
By doing this it ensures that all the provers are used equally and if one of them is stuck, it will be skipped until next round.
When it picks one it forces the lazy connection sets the state to `In_use` and sends the request.
If the connection is stuck or it failed to create, it will replace it with new lazy connection and move to the next prover.
This is done until `attempts` are exhausted.
If no prover is available it will wait for `wait_for_prover_timeout` seconds and throw.
If proving takes longer than `proving_timeout` and `attempts` are 0 it will throw.
