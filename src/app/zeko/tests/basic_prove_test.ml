open Snark_params.Tick

let ase, ase_proof =
  let open struct
    let trans0, proof0 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.With_length.leaf
        ( [ Field.one ]
        , { action_state = Field.of_string "6"
          ; length = Unsigned.UInt32.of_string "42"
          } )

    let trans1, proof1 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.With_length.leaf_option
        ([ Field.of_string "2" ], trans0.source)

    let trans2, proof2 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.With_length.merge
        { left = trans0
        ; left_proof = proof0
        ; right = trans1
        ; right_proof = proof1
        }

    let trans3, proof3 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.With_length.extend
        ([ Field.of_string "99" ], (trans2, proof2))

    let trans4, proof4 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.With_length.extend_option
        ([ Field.of_string "99" ], (trans3, proof3))
  end in
  (trans4, proof4)

let _ase_without_length =
  let open struct
    let trans0, proof0 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.Without_length.leaf ([ Field.one ], Field.of_string "6")

    let trans1, proof1 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.Without_length.leaf_option
        ([ Field.of_string "2" ], trans0.source)

    let trans2, proof2 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.Without_length.merge
        { left = trans0
        ; left_proof = proof0
        ; right = trans1
        ; right_proof = proof1
        }

    let trans3, proof3 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.Without_length.extend
        ([ Field.of_string "99" ], (trans2, proof2))

    let _trans4, _proof4 =
      Promise.block_on_async_exn
      @@ fun () ->
      Zeko_circuits.Ase.Without_length.extend_option
        ([ Field.of_string "99" ], (trans3, proof3))
  end in
  ()

let point_of_string s =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
  |> Signature_lib.Public_key.compress

let _inner =
  let open struct
    let Zeko_circuits.Compile_simple.[ sync; action ] =
      Zeko_circuits.Inner_rules.provers

    let ase : Zeko_circuits.Rule_inner_sync.Ase_inst.t =
      Zeko_circuits.Rule_inner_sync.Ase_inst.make ~proof_source:ase.source
        ~proof_target:ase.target ~proof:ase_proof ase.source
        [ Field.of_string "418923791273" ]

    let sync_witness : Zeko_circuits.Rule_inner_sync.Witness.t =
      { public_key = point_of_string "8184848488"
      ; vk_hash = Field.of_string "4819274123"
      ; ase
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> sync sync_witness

    let action_witness : Zeko_circuits.Rule_inner_action_witness.Witness.t =
      { public_key = point_of_string "8184848488"
      ; vk_hash = Field.of_string "4819274123"
      ; witness = { aux = Field.zero; children = [] }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> action action_witness
  end in
  ()
