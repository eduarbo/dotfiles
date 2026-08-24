# Personal Ops bootstrap

Clones or updates the canonical Personal Ops checkout, validates the control plane
and ecosystem contract, and reports whether the encrypted personal memory is present.

```bash
dot dev/personal-ops
```

The repository identity comes from `macos/workstation/profile.zsh`; it never depends
on `origin`. Dirty checkouts are preserved and never pulled. The topic does not copy
credentials, initialize replacement memory, enable writers or change automations.

Machine-local overrides:

- `PERSONAL_OPS_REPOSITORY`
- `PERSONAL_OPS_DIR`
- `WORKSTATION_REPOSITORY_OWNER`
