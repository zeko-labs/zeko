Feature: Explorer End-to-End Publishing

  Scenario: Sequencer-applied transactions are published to NATS
    Given the Zeko integration services are running with a NATS broker
    When the sequencer applies a user transaction
    Then an explorer subscriber receives a "user_command" transaction event
    And the transaction event includes the target ledger hash as "Nats-Msg-Id"

  Scenario: Backfill replays sequencer DA diffs to NATS
    Given a sequencer-applied transaction has been stored in the DA layer
    When the backfill service replays from genesis to that transaction
    Then an explorer subscriber receives a replay event for that target ledger hash
