# GitHub SSH profiles and multi-login

GitHub authenticates an SSH connection by key before it knows which repository
will be accessed. If an agent exposes several keys, GitHub can accept the first
recognized key and OpenSSH never reaches the intended one. These dotfiles solve
that ambiguity by binding exactly one public key to each repository through its
local Git configuration:

```text
remote.origin.url = git@github.com:OWNER/REPO.git
githubAccount.id  = account-1
core.sshCommand   = ~/.config/github-accounts/bin/account-1-ssh
```

An `account-N` is a **local SSH/Git profile**, not necessarily a distinct GitHub
login. Multiple profiles can use different keys for the same GitHub login,
while a client or bot login can use a separate profile. IDs stay generic so
they reveal no person, organization, customer, device, or purpose.

The repository-local binding selects four things. Its small SSH selector adds
the profile key only for GitHub and passes other SSH hosts through unchanged:

- the only SSH authentication key OpenSSH may offer;
- the local commit author and email;
- the optional SSH commit-signing key;
- the GitHub CLI policy: isolated, shared, or disabled.

GitHub does not expose an organization-to-key mapping during SSH
authentication, so it cannot be inferred reliably. Associating a repository
with a local profile is the one required decision. Remotes remain canonical;
no hostname alias or global URL rewrite attempts to guess the profile.

## GitHub CLI policies

Each profile records one of these local-only policies:

| Policy | Intended use | Browser login |
| --- | --- | --- |
| `isolated` | A distinct GitHub login, such as a future client account | Once for that login |
| `shared` | Another SSH key/profile for the same GitHub login | Reuses the owner profile; no second session |
| `none` | SSH-only bot or automation profile that does not need `gh` | Never |

Sharing is accepted only when both profiles have exactly the same GitHub login.
The shared profile must point directly to an isolated owner profile; chains and
cross-login sharing fail validation. This keeps session resolution explicit and
prevents accidental API actions as another account.

`GH_CONFIG_DIR` isolates each owner session. During `gh auth login`, an isolated
`GIT_CONFIG_GLOBAL` also prevents GitHub CLI from installing credential helpers
in the real global Git configuration. The normal `gh` executable is not wrapped,
shadowed, or globally switched.

## Bootstrap

Deploy the Git and SSH topics and inspect the keys currently exposed by the
agent:

```sh
dot shell/git shell/ssh
git account discover
git account add
```

`discover` makes a real SSH authentication attempt for every agent key and
reports the association returned by GitHub. It displays only a transient index,
fingerprint, and detected login. It persists nothing and never reads or prints
key comments.

The interactive `add` flow stores a comment-free public key under
`~/.config/github-accounts/`, asks GitHub which login owns it when possible, and
prompts only for values that cannot be inferred safely. Use the transient
indices without copying key material:

```sh
git account add account-1 \
  --agent-index N \
  --signing-agent-index M
```

By default the authentication key is also used for SSH commit signing. Use
`--signing-key /path/to/public-key` for a separate key or `--no-sign` to opt out.
GitHub must know a signing public key with key type **Signing** for commits to
appear verified on the website. This tool never uploads, deletes, or revokes a
key.

### First GitHub login

The first profile for a login defaults to an isolated `gh` session. Import an
already authenticated normal GitHub CLI session without displaying its token:

```sh
git account import-gh account-1
```

If no matching session exists, authenticate through the normal device flow:

```sh
git account login account-1
```

### Another key for the same GitHub login

Add another profile with the intended authentication/signing keys:

```sh
git account add account-2 --agent-index N --signing-agent-index M
```

When GitHub reports the same login as an existing isolated profile, `add`
automatically records `gh=shared` and does not require another browser login.
The explicit equivalent is:

```sh
git account add account-2 --share-gh account-1
```

This is appropriate for separate personal, organization, or customer-context
keys that all belong to the same GitHub login. The organization itself is not
stored and cannot be inferred from the key.

### SSH-only bot

A bot login that only pushes or clones over SSH does not need a GitHub CLI
session:

```sh
git account add account-2 --no-gh
```

`doctor` treats this as intentional and `test` automatically skips only the API
check. No device code or browser login is requested. If API operations become
necessary later, opt in explicitly:

```sh
git account add account-2 --gh isolated
git account login account-2
```

### Future client login

For a key that GitHub associates with a different login, a new profile defaults
to its own isolated `gh` session. Authenticate it only if GitHub CLI access is
needed. If the work is SSH-only, use `--no-gh` instead.

Re-running `add` is idempotent. Existing values and keys are retained unless
replacement options are supplied. `render` validates profiles and removes files
from the retired hostname-alias design. Inspect generic state without printing
usernames or emails:

```sh
git account list
```

## Associate repositories

For an existing repository, explicitly associate one remote once:

```sh
git account use account-1
```

This normalizes `remote.origin.url` to `git@github.com:OWNER/REPO.git` and writes
the selected SSH key, Git identity, and signing settings only to `.git/config`.
Pass a remote name as the second argument when it is not `origin`. `migrate` is
an alias for `use` when converting repositories from an older setup.

Clone a new repository through a profile while keeping the stored remote
canonical:

```sh
git account clone account-1 OWNER/REPO
```

The clone uses the selected key for its first fetch and then records the same
repository-local binding. Keep read-only upstream remotes on HTTPS when they do
not need authentication. Every authenticated SSH remote in one repository uses
the repository's single selected profile.

Unbound `git@github.com` authentication is intentionally fail-closed. Ordinary
public HTTPS clones remain HTTPS, and there is no global `insteadOf` conversion.
For an organization protected by SAML SSO, GitHub may additionally require the
selected key to be authorized for that organization; that association lives on
GitHub and is never guessed or modified here.

## Run GitHub CLI

Run GitHub CLI with the isolated or shared session selected by the current
repository:

```sh
git account gh -- pr status
git account gh -- repo view
```

Outside a matching repository, name the generic profile:

```sh
git account gh account-2 -- auth status
```

Profiles with `gh=none` reject this command with a clear error. Using an explicit
subcommand instead of shadowing `gh` keeps scripts independent of the current
directory.

## Diagnostics and real tests

Every push runs the managed Gitleaks pre-push hook against the commits being
published. The hook blocks the push if Gitleaks is missing, errors, or detects
a secret; output is always redacted. Install or update this topic on each
machine to keep the scanner and hook active.

Local checks make no network request:

```sh
git account doctor
```

They validate repository-local SSH selection, agent availability, signing-key
availability, the `gh` policy and exact session association, fail-closed
unbound GitHub SSH, canonical remotes, and the absence of global URL rewrites.

The integration test performs real SSH authentication, checks the GitHub API
when enabled, creates and verifies a temporary signed commit, and makes a real
shallow clone into a temporary directory:

```sh
git account test account-1 OWNER/REPO
```

Inside a migrated repository, both the profile and `OWNER/REPO` are inferred:

```sh
git account test
```

For an isolated profile whose `gh` login has not been completed, the API layer
can be skipped explicitly:

```sh
git account test --skip-gh account-2 OWNER/REPO
```

For `gh=none`, that skip is automatic. Unit tests for clean deployment,
canonical remotes, repository-local identities, shared and disabled `gh`
policies, login sandboxing, and legacy cleanup are available at:

```sh
shell/git/tests/git-account-test
```

## Local-only data

None of these paths is linked into or generated inside the dotfiles repository:

```text
~/.config/github-accounts/accounts/   profile metadata and gh policy
~/.config/github-accounts/bin/        per-profile SSH selectors
~/.config/github-accounts/keys/       comment-free public keys
~/.config/github-accounts/gh/         isolated owner sessions for GitHub CLI
~/.config/git/local.gitconfig         other machine-local Git settings only
~/.config/ssh/local.conf              other machine-local SSH hosts
```

Directories use mode `0700`; generated files use mode `0600`. Existing local
data is preserved when the Git or SSH topic is disabled.

Repository bindings live in each repository's untracked `.git/config`.
`git account render` validates profile data and removes obsolete generated
alias/routing files; it is safe to run repeatedly. Back up
`~/.config/github-accounts/` only through an encrypted local backup; never copy
it into this repository.

Removing PII or public keys from the current tree does not erase them from older
Git commits. This setup deliberately performs no history rewrite, force-push,
key upload, key deletion, or key revocation.

## Troubleshooting

- **A bot asks for a device code:** disable GitHub CLI for that profile with
  `git account add account-N --no-gh`. SSH, Git identity, signing, and cloning
  remain active.
- **Two keys return the same login:** this is expected for multiple keys on one
  GitHub account. Let the newer profile share the isolated owner with
  `--share-gh account-N`; choose the intended profile per repository.
- **SSH reports a different login:** run `git account discover`, then update the
  profile with the correct `--agent-index`. `test` compares GitHub's greeting to
  the locally recorded login and fails on a mismatch.
- **`gh` opens the wrong browser account:** cancel the flow rather than approving
  it. For a distinct login, sign into the login recorded for that profile; for
  the same login, use `--share-gh`; for SSH-only automation, use `--no-gh`.
- **Git reports an unbound SSH remote:** run `git account use account-N` in that
  repository. Keep read-only upstream remotes on HTTPS when possible.
- **Signing works locally but GitHub does not show Verified:** register the
  public signing key with key type **Signing** on the same GitHub login. Keys are
  never uploaded automatically.
- **The agent key is unavailable:** unlock Bitwarden, enable its SSH agent, and
  confirm `SSH_AUTH_SOCK` points to the Bitwarden socket before running `doctor`
  again.
- **A repository uses the wrong key or identity:** rerun
  `git account use account-N`. It rewrites only that repository's local binding
  and leaves its canonical remote intact.
