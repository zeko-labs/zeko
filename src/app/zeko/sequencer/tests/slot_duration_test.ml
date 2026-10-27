open Core_kernel

let slot_at ~slot_duration_sec =
  let l1_config : Utils.Slot.l1_config =
    { fork_timestamp = Time.epoch
    ; fork_slot = Mina_numbers.Global_slot_since_genesis.of_int 100
    ; slot_duration_sec
    }
  in
  Utils.Slot.global_slot_at
    ~now:(Time.add Time.epoch (Time.Span.of_sec 360.))
    ~l1_config
  |> Mina_numbers.Global_slot_since_genesis.to_int

let () =
  assert (slot_at ~slot_duration_sec:180 = 102) ;
  assert (slot_at ~slot_duration_sec:12 = 130)
