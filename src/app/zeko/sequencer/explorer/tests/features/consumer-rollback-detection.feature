Feature: Consumer Rollback Detection

  Scenario: Indexer detects hash chain break as rollback
    Given a NATS stream containing messages with continuous hash chain A->B->C->D
    When a new message arrives with source_ledger_hash equal to B (not D)
    Then the indexer identifies messages C and D as rolled back
    And the indexer reverts SurrealDB state for transactions C and D
    And the indexer continues processing from the new message

  Scenario: Indexer detects gap in hash chain
    Given a NATS stream containing messages with hash chain A->B
    When a new message arrives with source_ledger_hash equal to E (unknown)
    Then the indexer pauses processing
    And the indexer requests backfill from the backfill API for hashes B to E
    And the indexer resumes after backfill completes

  Scenario: Indexer maintains hash-to-sequence mapping for ancestor lookup
    Given the indexer has processed 1000 messages
    When a hash chain break is detected
    Then the indexer looks up the common ancestor hash in its mapping
    And the lookup completes in O(1) time
    And the indexer knows exactly which messages to revert
