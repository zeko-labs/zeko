Feature: Backfill Service

  Scenario: Backfill mutation fills a gap in the NATS stream
    Given a backfill service running with --nats-url and --da-nodes
    And the NATS stream has messages A->B and D->E (gap: B to D)
    When the indexer calls mutation backfill(fromHash: "B", toHash: "D")
    Then the backfill service fetches diffs B->C and C->D from the DA layer
    And publishes them to "zeko.l2.transactions" with correct Nats-Msg-Id headers
    And the NATS stream now has A->B->C->D->E with no gaps

  Scenario: Backfill handles full bootstrap from genesis
    Given a backfill service running with --nats-url and --da-nodes
    And an empty NATS stream
    When the indexer calls mutation backfill(fromHash: "genesis", toHash: "current_committed")
    Then the backfill service streams all diffs from genesis to committed state
    And publishes them in order to "zeko.l2.transactions"
    And each message has a unique Nats-Msg-Id header

  Scenario: Backfill deduplicates with existing messages
    Given a NATS stream that already contains messages A->B->C
    When the backfill service publishes diffs for the range A->C
    Then JetStream deduplicates messages with matching Nats-Msg-Id headers
    And no duplicate messages appear in the stream

  Scenario: SSE subscription streams progress in real time
    Given a backfill service running with --nats-url and --da-nodes
    When the indexer calls mutation backfill(fromHash: "A", toHash: "D") for a range with 3 diffs
    And subscribes to backfillProgress(id) via SSE
    Then the indexer receives progress events with increasing diffsPublished counts
    And the final event has status "COMPLETED" and diffsPublished equal to 3

  Scenario: Indexer recovers from SSE connection drop
    Given a backfill job is running with id "job-1"
    And the indexer is subscribed to backfillProgress(id: "job-1") via SSE
    When the SSE connection drops unexpectedly
    Then the indexer queries backfillJob(id: "job-1") to check current status
    And if status is "RUNNING", the indexer re-subscribes to backfillProgress(id: "job-1")
    And if status is "COMPLETED", the indexer resumes NATS consumption
    And if status is "FAILED", the indexer retries the backfill mutation

  Scenario: Indexer detects backfill service restart via instanceId
    Given a backfill service running with instanceId "abc-123"
    And the indexer has submitted a backfill job and is subscribed via SSE
    When the backfill service restarts with a new instanceId "def-456"
    And the SSE connection drops
    Then the indexer queries health { instanceId }
    And detects the instanceId changed from "abc-123" to "def-456"
    And re-submits the backfill mutation with the original hash range
    And JetStream deduplication prevents duplicate messages for already-published diffs

  Scenario: Indexer detects backfill service restart via null job
    Given a backfill service running with instanceId "abc-123"
    And the indexer has a backfill job with id "job-1"
    When the backfill service restarts
    And the indexer queries backfillJob(id: "job-1")
    Then the query returns null (job lost from in-memory storage)
    And the indexer re-submits the backfill mutation

  Scenario: Backfill mutation rejects invalid hash ranges
    Given a backfill service running with --nats-url and --da-nodes
    When the indexer calls mutation backfill(fromHash: "nonexistent", toHash: "D")
    Then the mutation returns a BackfillJob with status "FAILED" and a descriptive error

  Scenario: Indexer retries with exponential backoff when service is unreachable
    Given a backfill service that is temporarily down
    When the indexer attempts to call the backfill mutation
    Then the request fails with a connection error
    And the indexer retries with exponential backoff (1s, 2s, 4s, ...)
    And the indexer remains paused (not consuming from NATS) until backfill succeeds
