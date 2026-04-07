Feature: NATS Downtime Resilience

  Scenario: Sequencer continues when NATS is unavailable
    Given a sequencer running with --nats-url nats://localhost:4222
    And the NATS server is stopped
    When a transaction is submitted to the sequencer
    Then the transaction is applied successfully
    And the diff is posted to the DA layer
    And no NATS message is published

  Scenario: Sequencer resumes publishing after NATS reconnects
    Given a sequencer running with --nats-url nats://localhost:4222
    And the NATS server was stopped and then restarted
    When a new transaction is submitted to the sequencer
    Then the NATS client reconnects automatically
    And the new transaction's diff is published to "zeko.l2.transactions"

  Scenario: Sequencer starts without NATS flag
    Given a sequencer running without the --nats-url flag
    When a transaction is submitted to the sequencer
    Then the transaction is applied successfully
    And no NATS connection is attempted
