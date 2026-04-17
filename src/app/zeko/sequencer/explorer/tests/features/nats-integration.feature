Feature: Explorer NATS Integration

  Scenario: Live transaction events round-trip through NATS with the dedup header
    Given a running NATS server
    When the explorer publisher emits a live transaction event
    Then a subscriber receives it on "zeko.l2.transactions"
    And the message includes the "Nats-Msg-Id" header
    And the payload kind is "user_command"

  Scenario: Backfill replay events round-trip through NATS as genesis replays
    Given a running NATS server
    When the backfill service republishes the first genesis diff
    Then a subscriber receives it on "zeko.l2.transactions"
    And the payload kind is "genesis_replay"
    And the payload marks genesis as true

  Scenario: JetStream deduplicates transaction publishes by Nats-Msg-Id
    Given a running NATS server with JetStream enabled
    When the same transaction event is published twice with the same "Nats-Msg-Id"
    Then the JetStream stream stores one transaction message
