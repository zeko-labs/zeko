Feature: Finality Event Publishing

  Scenario: Proved finality event is published after merger completes
    Given a sequencer running with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-l2" subscribed to "zeko.l2.>"
    When the merger tree produces a completed proof
    Then a message is published to "zeko.l2.finality"
    And the payload "level" is "proved"
    And the payload "ledger_hash" matches the proved state's target ledger
    And the payload "source_ledger_hash" matches the proved state's source ledger

  Scenario: Committed finality event is published after L1 submission
    Given a sequencer running with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-l2" subscribed to "zeko.l2.>"
    When a commit is successfully submitted to L1
    Then a message is published to "zeko.l2.finality"
    And the payload "level" is "committed"
    And the payload "ledger_hash" matches the committed ledger hash
