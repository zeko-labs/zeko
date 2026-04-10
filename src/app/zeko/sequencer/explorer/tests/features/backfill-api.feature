Feature: Explorer Backfill API

  Scenario: A genesis backfill marks only the first replayed diff as genesis
    Given the backfill service is replaying from genesis
    When it classifies the first replayed diff
    Then the diff kind is "genesis_replay"
    And later replayed diffs use "sync_replay"

  Scenario: The backfill mutation returns a failed job snapshot for invalid hashes
    Given a standalone backfill service
    When GraphQL mutation backfill is called with an invalid fromHash
    Then the mutation returns a BackfillJob snapshot
    And the snapshot status is "failed"
    And the snapshot error mentions an invalid ledger hash

  Scenario: Backfill progress subscriptions stream job updates
    Given a standalone backfill service with a queued backfill job
    When the job publishes progress updates
    Then the subscription yields increasing diffsPublished counts
    And the final update has status "completed"

  Scenario: The backfill health query exposes the service instance
    Given a standalone backfill service
    When GraphQL query health is executed
    Then the response includes instanceId
    And the response includes startedAt

  Scenario: The backfill job query returns the current job snapshot
    Given a standalone backfill service with a stored backfill job
    When GraphQL query backfillJob is executed for that id
    Then the response includes the matching job id
    And the response includes the job status

  Scenario: Backfill progress subscriptions stream GraphQL-SSE events
    Given a standalone backfill service with a queued backfill job
    When the GraphQL-SSE endpoint subscribes to that job
    Then the first SSE event includes diffsPublished 0
    And later SSE events reflect published progress
    And the stream finishes with a complete event
