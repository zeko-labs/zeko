Feature: Explorer Event Contract

  Scenario: Transaction events include the replay kind, diff payload, and NATS dedup header
    Given a transaction diff prepared for explorer publishing
    When the transaction event message is built
    Then the subject is "zeko.l2.transactions"
    And the payload includes "kind"
    And the payload includes "target_ledger_hash"
    And the payload includes "diff"
    And the headers include "Nats-Msg-Id"

  Scenario: Finality events include status and ledger hashes
    Given a finality transition prepared for explorer publishing
    When the finality event message is built
    Then the subject is "zeko.l2.finality"
    And the payload includes "status"
    And the payload includes "source_ledger_hash"
    And the payload includes "target_ledger_hash"

  Scenario: Health events include the service identity and publishing state
    Given sequencer health data prepared for explorer publishing
    When the health event message is built
    Then the subject is "zeko.health"
    And the payload includes "service"
    And the payload includes "instance_id"
    And the payload includes "last_published_hash"
    And the payload includes "unproved_hash"
