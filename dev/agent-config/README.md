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

The topic also installs a daily `agent-config doctor` monitor and runs an
immediate audit. Use `agent-config sync -m "reason"` after intentional changes;
it validates, commits, rebases concurrent work from another machine, and pushes
without force. Real conflicts stop for explicit resolution.

The dotfiles repository is registered for read-only daily auditing. Dirty,
unpushed, behind, or divergent state triggers a notification, but is never
auto-committed by `agent-config sync`.

When `git account` is configured, the topic maps the local GitHub login to its
PII-free `account-N` alias automatically. Authentication, author identity, and
signing remain machine-local and work independently on every computer.

Optional machine-local overrides:

- `AGENT_CONFIG_ACCOUNT`
- `AGENT_CONFIG_GLOBAL_REPO` and `AGENT_CONFIG_OWNER_REPO`
- `AGENT_CONFIG_GLOBAL_DIR` and `AGENT_CONFIG_OWNER_DIR`
- `AGENT_WORKSPACES_DIR`
