Feature: NATS Message Deduplication

  Scenario: Duplicate messages are deduplicated by JetStream
    Given a sequencer running with --nats-url nats://localhost:4222
    And a NATS JetStream stream "zeko-l2" with dedup window of 2 minutes
    When the sequencer publishes two messages with the same Nats-Msg-Id header
    Then only one message appears in the stream
