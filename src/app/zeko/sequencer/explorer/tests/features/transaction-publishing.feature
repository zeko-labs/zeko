Feature: Transaction Publishing to NATS

  Scenario: User command is published to NATS
    Given a sequencer running with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-l2" subscribed to "zeko.l2.>"
    When a signed command is submitted to the sequencer
    Then a message is published to "zeko.l2.transactions"
    And the message contains a valid JSON payload
    And the payload contains "source_ledger_hash" as a non-empty string
    And the payload contains "target_ledger_hash" as a non-empty string
    And the payload contains "changed_accounts" as a non-empty array
    And the payload "command" field is not null
    And the payload "command.type" is "signed_command"
    And the message has a "Nats-Msg-Id" header equal to the "target_ledger_hash"

  Scenario: zkApp command is published to NATS
    Given a sequencer running with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-l2" subscribed to "zeko.l2.>"
    When a zkApp command is submitted to the sequencer
    Then a message is published to "zeko.l2.transactions"
    And the payload "command.type" is "zkapp_command"
    And the payload "command.account_updates" is a non-empty array

  Scenario: Fee transfer is published to NATS
    Given a sequencer running with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-l2" subscribed to "zeko.l2.>"
    When a commit period triggers a fee transfer
    Then a message is published to "zeko.l2.transactions"
    And the payload "command" field is null
    And the payload "changed_accounts" contains the sequencer's account

  Scenario: Genesis diffs are published on startup sync
    Given a sequencer starting fresh with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-l2" subscribed to "zeko.l2.>"
    When the sequencer syncs from L1 committed state
    Then one or more messages are published to "zeko.l2.transactions"
    And the first message has "genesis" set to true

  Scenario: Ledger hash chain continuity
    Given a sequencer running with --nats-url nats://localhost:4222
    And multiple transactions have been published to "zeko.l2.transactions"
    When reading messages in sequence order
    Then each message's "target_ledger_hash" equals the next message's "source_ledger_hash"
