Feature: Sequencer Explorer Hooks

  Scenario: Sync replay from genesis marks the very first diff as genesis
    Given the sequencer is replaying diffs from genesis
    When it classifies the first replayed diff
    Then the replay is marked as genesis

  Scenario: Sync replay from a checkpoint never re-labels diffs as genesis
    Given the sequencer is replaying diffs from a checkpoint
    When it classifies the first replayed diff
    Then the replay is not marked as genesis
