open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Bridge_state

module Make_mina (Inputs : sig
  val holder_accounts_l1 : PC.t list

  val holder_account_l2 : PC.t

  val zeko_l1 : PC.t

  val zeko_l2 : PC.t

  val withdrawal_delay : Mina_numbers.Global_slot_span.t

  val holder_account_l1_permissions_enabled : Mina_base.Permissions.t

  val holder_account_l1_permissions_disabled : Mina_base.Permissions.t
end)
() =
struct
  module Check_accepted =
    Check_accepted_make.Make
      (struct
        let holder_accounts_l1 = Inputs.holder_accounts_l1

        let token_owner_l1 = None

        module Deposit_params = Deposit_params_base
      end)
      ()

  module Inputs = struct
    include Inputs

    let token_owner_l1 = None

    let token_owner_l2 = None

    module Deposit_params = Deposit_params_base
    module Withdrawal_params = Withdrawal_params_base
    module Check_accepted = Check_accepted
  end

  module Rule_bridge_finalize_deposit =
    Rule_bridge_finalize_deposit.Make (Inputs)

  module Rule_bridge_finalize_cancelled_deposit =
    Rule_bridge_finalize_cancelled_deposit.Make (Inputs) ()

  module Rule_bridge_finalize_withdrawal =
    Rule_bridge_finalize_withdrawal.Make (Inputs)
  module Rule_bridge_disable = Rule_bridge_disable.Make (Inputs)
  module Rule_bridge_enable = Rule_bridge_enable.Make (Inputs)

  module System_L1 =
  ( val Compile_simple.compile ~name:"bridge rules for mina l1"
          ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~branches:
            [ Rule_bridge_finalize_cancelled_deposit.rule
            ; Rule_bridge_finalize_withdrawal.rule
            ; Rule_bridge_disable.rule
            ; Rule_bridge_enable.rule
            ]
          () )

  module System_L2 =
  ( val Compile_simple.compile ~name:"bridge rules for mina l2"
          ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~branches:[ Rule_bridge_finalize_deposit.rule ]
          () )
end

module Make_custom (Inputs : sig
  val token_owner_l1 : Account_id.t

  val token_owner_l2 : Account_id.t

  val holder_accounts_l1 : PC.t list

  val zeko_l1 : PC.t

  val zeko_l2 : PC.t

  val holder_account_l2 : PC.t

  val withdrawal_delay : Mina_numbers.Global_slot_span.t

  val holder_account_l1_permissions_enabled : Mina_base.Permissions.t

  val holder_account_l1_permissions_disabled : Mina_base.Permissions.t
end)
() =
struct
  module Check_accepted =
    Check_accepted_make.Make
      (struct
        let holder_accounts_l1 = Inputs.holder_accounts_l1

        let token_owner_l1 = Some Inputs.token_owner_l1

        module Deposit_params = Deposit_params_custom
      end)
      ()

  module Inputs = struct
    include Inputs

    let token_owner_l1 = Some Inputs.token_owner_l1

    let token_owner_l2 = Some Inputs.token_owner_l2

    module Deposit_params = Deposit_params_custom
    module Withdrawal_params = Withdrawal_params_custom
    module Check_accepted = Check_accepted
  end

  module Rule_bridge_finalize_deposit =
    Rule_bridge_finalize_deposit.Make (Inputs)

  module Rule_bridge_finalize_cancelled_deposit =
    Rule_bridge_finalize_cancelled_deposit.Make (Inputs) ()

  module Rule_bridge_finalize_withdrawal =
    Rule_bridge_finalize_withdrawal.Make (Inputs)
  module Rule_bridge_disable = Rule_bridge_disable.Make (Inputs)
  module Rule_bridge_enable = Rule_bridge_enable.Make (Inputs)

  module System_L1 =
  ( val Compile_simple.compile ~name:"bridge rules for custom l1"
          ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~branches:
            [ Rule_bridge_finalize_cancelled_deposit.rule
            ; Rule_bridge_finalize_withdrawal.rule
            ; Rule_bridge_disable.rule
            ; Rule_bridge_enable.rule
            ]
          () )

  module System_L2 =
  ( val Compile_simple.compile ~name:"bridge rules for custom l2"
          ~out_typ:Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~branches:[ Rule_bridge_finalize_deposit.rule ]
          () )
end
