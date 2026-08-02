# Agent configuration

Bootstraps the private `agent-config-global` repository and its optional
owner-level `agent-config` companion, then runs the global setup.

```bash
dot dev/agent-config
```

The GitHub account is inferred from this dotfiles checkout's `origin`, not from
whichever account happens to be active in SSH. Cloning uses the matching account
stored by `gh`, over HTTPS, so a fresh install does not depend on SSH routing.

Local changes are never overwritten: dirty repositories are not pulled,
unmanaged files are not replaced, and removing the topic preserves both the
repositories and installed links.

Optional machine-local overrides:

- `AGENT_CONFIG_ACCOUNT`
- `AGENT_CONFIG_GLOBAL_REPO` and `AGENT_CONFIG_OWNER_REPO`
- `AGENT_CONFIG_GLOBAL_DIR` and `AGENT_CONFIG_OWNER_DIR`
- `AGENT_WORKSPACES_DIR`
