Feature: Health Heartbeat

  Scenario: Sequencer publishes periodic health heartbeats
    Given a sequencer running with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-health" subscribed to "zeko.health"
    When the health heartbeat interval elapses
    Then a message is published to "zeko.health"
    And the payload contains "service" as "sequencer-nats-publisher"
    And the payload contains "last_published_hash"
    And the payload contains "unproved_hash"
