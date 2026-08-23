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

On a fresh Mac, the dependency graph installs missing components before the
postflight runs. On later runs, this topic validates readiness; it does not perform
a recursive upgrade of already-enabled dependencies. Update an owner directly when
needed, then rerun `dot macos/workstation` to validate the complete profile.

The postflight checks the expected topics, Codex and Claude CLIs, ChatGPT and Claude
desktop apps, managed agent-configuration links, and managed repositories without
network access.

Authentication, macOS permissions, SSH/GPG/Keychain material, Personal Ops memory,
and writer cutover remain explicit machine-local gates. This topic never copies auth
stores, initializes memory, or enables Personal Ops writers or automations.
