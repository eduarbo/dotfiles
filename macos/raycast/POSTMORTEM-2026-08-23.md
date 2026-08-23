# Raycast recovery post mortem — 2026-08-23

## Summary

The Raycast recovery was prepared from the correct encrypted snapshot but was not
initially completed. The workflow stopped at the password prompt, yet the task was
handed back without preserving an unmistakable `waiting` status and without a
post-handoff functional test. The missing launcher shortcut exposed the incomplete
outcome.

No Raycast data was lost. The import was subsequently completed from the versioned
snapshot and verified in Raycast.

## Impact

- The normal launcher workflow remained unavailable longer than necessary.
- Eduardo had to detect and report that the shortcut still failed.
- Preparation of the recovery was easy to confuse with completion of the recovery.

## Evidence

- The versioned `raycast.rayconfig` matched its repository SHA-256 before import.
- Raycast remained on `Enter your password` with `Import` disabled, proving no
  import had occurred at the first handoff.
- After the completed import, Raycast reported the launcher hotkey as `F16`.
- A synthetic `F16` invocation opened Raycast successfully.
- Restored Store extensions were visible, providing a second independent readback.
- The active Karabiner configuration maps a tap of Right Option to `F16`, and the
  Karabiner services were running.

## Root cause

The workflow tracked the last attempted action—opening and preparing the native
importer—instead of the requested outcome: restored settings working through the
user's normal launcher path. A credential handoff ended the turn without a mandatory
state transition back from `waiting` to verified success.

## Contributing factors

- Raycast intentionally has no headless import; the helper can only prepare the UI.
- Screen Recording permissions required a Codex restart mid-recovery.
- The password boundary made a human handoff reasonable, but the status language was
  not strong enough to prevent it from feeling like a completed delivery.
- The initial acceptance check did not include the real hotkey or a second restored
  item.

## Corrective actions

1. Codex global instructions now distinguish prepared or submitted actions from
   achieved outcomes.
2. A pending credential, restart, confirmation or human action must remain explicitly
   `waiting`, `partial` or `blocked`.
3. After a handoff, Codex must re-read authoritative state and run an outcome-level
   functional test before declaring success.
4. The gate cannot be used to expand authority or justify accessing secrets.
5. `raycast-import` now labels its own result as `PREPARED ONLY` and prints the
   recovery acceptance checks.

## Recovery acceptance gate

A Raycast recovery is complete only when all of these are true:

1. The intended snapshot is identified and its hash is verified.
2. The selected categories match the authorized recovery scope.
3. Raycast accepts the passphrase and finishes without an error.
4. The import prompt closes and Raycast returns to an operational state.
5. The configured launcher hotkey is read back and functionally opens Raycast.
6. At least one additional restored alias, Quicklink, Snippet or extension is visible.

If any item is missing, report the recovery as incomplete and name the exact owner and
next action.
