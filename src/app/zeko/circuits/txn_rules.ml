include
  ( val Compile_simple.compile ~name:"zeko-transaction-snark"
          ~out_typ:Txn_state.Zeko_stmt.typ
          ~branches:
            [ { branch_name = "single-signed-command"
              ; tags = No_tags
              ; main = Rule_signed_command.main
              }
            ; { branch_name = "single-unproved-zkapp-command"
              ; tags = No_tags
              ; main = Rule_zkapp_command.single_unproved
              }
            ; { branch_name = "double-unproved-zkapp-command"
              ; tags = No_tags
              ; main = Rule_zkapp_command.double_unproved
              }
            ; { branch_name = "single-proved-zkapp-command"
              ; tags =
                  One_tag_sideloaded
                    { sideloaded_tag_name =
                        "single-proved-zkapp-command-sideloaded-vk"
                    ; typ = Mina_base.Zkapp_statement.typ
                    ; extract_vk =
                        (fun ({ vk; _ } :
                               Rule_zkapp_command.Zkapp_single_proved_input.t ) ->
                          vk )
                    }
              ; main = Rule_zkapp_command.single_proved
              }
            ; { branch_name = "merge"
              ; tags = Two_tags_own
              ; main = Rule_txn_merge.main
              }
            ]
          () )

let make = make_unchecked
