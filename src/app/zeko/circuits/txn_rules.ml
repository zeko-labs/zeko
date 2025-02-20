include
  ( val Compile_simple.compile ~name:"zeko-transaction-snark"
          ~out_typ:Zeko_stmt.typ
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
                        (fun ({ zkapp_vk; _ } : Zkapp_single_proved_input.t) ->
                          Compile_simple.Verification_key.of_pickles zkapp_vk )
                    }
              ; main = Rule_zkapp_command.single_proved
              }
            ; { branch_name = "merge"; tags = Two_tags_own; main = Rule_txn_merge.main }
            ]
          () )
