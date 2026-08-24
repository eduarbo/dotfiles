# macOS workstation

Bootstrap and validate the personal Mac profile through one topic:

```bash
dot macos/workstation
```

This is a composition and readiness topic. It delegates package and configuration
ownership to:

- `macos/apps` for personal desktop apps and its dedicated app/CLI topics.
- `dev/agent-config` for managed instructions, skills, repositories, links, and
  health checks.
- `dev/personal-ops` for the control plane, ecosystem contract and encrypted-memory
  readiness gate.

On a fresh Mac, the dependency graph installs missing components before the
postflight runs. On later runs, this topic updates each owner in dependency order and
then validates the whole profile. Dirty repositories fail closed or are preserved by
their owner; no workflow overwrites unmanaged state.

`workstation-doctor --json` checks CLIs, apps, managed links, Personal Ops, the
ecosystem contract and encrypted-memory presence without network access. It also lists
the manual gates instead of claiming that login, iCloud or iPhone are verified.

The same mechanism supports both modes:

- Full personal profile: `dot macos/workstation`.
- Selective setup: `dot TOPIC...`; only the selected topics and their dependencies.
- Exact dry-run: `dot -t macos/workstation` or `dot -t TOPIC...`.

Authentication, macOS permissions, SSH/GPG/Keychain material, Personal Ops memory,
and writer cutover remain explicit machine-local gates. This topic never copies auth
stores, initializes memory, or enables Personal Ops writers or automations.
