# GitHub multi-account setup

GitHub identifies an SSH connection by its key, not by the repository path.
When an agent exposes several keys, GitHub can accept the first recognized key
before OpenSSH tries the intended one. This setup therefore gives every account
a generic SSH hostname and exactly one public key:

```text
github-account-1       -> github.com, account-1 authentication key only
github-account-1-gist  -> gist.github.com, same authentication key only
github-account-2       -> github.com, account-2 authentication key only
```

The generic alias in a repository remote is the only durable account selector.
Git uses that selector to include the matching local author, email, and SSH
signing configuration. GitHub CLI state is isolated per account with
`GH_CONFIG_DIR`; the normal `gh` executable is not replaced and its active
global account is never switched. Authentication also receives an isolated
`GIT_CONFIG_GLOBAL`, because `gh auth login` otherwise installs credential
helpers in the real global Git configuration even when Git operations use SSH.

## Bootstrap

Deploy the Git and SSH topics, then add the first account:

```sh
dot shell/git shell/ssh
git account discover
git account add
```

`discover` makes a real SSH authentication attempt for every agent key and
reports the association GitHub returns. It displays only a transient index,
fingerprint, and detected login; it does not persist anything and never reads or
prints key comments.

The interactive `add` flow also lists agent keys by transient fingerprint. It
stores a comment-free public key under
`~/.config/github-accounts/`, asks GitHub which login owns that key when
possible, and prompts only for data that cannot be inferred safely. Account IDs
are always generic (`account-N`).

By default the authentication key is also used for SSH commit signing. Pass
`--signing-key /path/to/public-key` for a separate signing key, or `--no-sign`
to opt out. GitHub must know that public key as a **signing** key for commits to
appear verified on the website; this tool does not upload or revoke keys.
When both keys are already in the agent, use `--agent-index N` and
`--signing-agent-index N` with the transient indices from `discover`.

Authenticate the matching isolated `gh` session. If the same login already
exists in the normal `gh` configuration, import it without displaying its
token:

```sh
git account import-gh account-1
```

Otherwise use the normal browser login:

```sh
git account login account-1
```

Repeat `git account add` for additional accounts. Re-running `add` or `render`
is idempotent.

## Repositories

For an existing repository, explicitly associate its remote once:

```sh
git account migrate account-1
```

This changes only `remote.origin.url`, for example from HTTPS or
`git@github.com:OWNER/REPO.git` to
`git@github-account-1:OWNER/REPO.git`. Pass a remote name as the second argument
when it is not `origin`. Mapping every repository automatically would be unsafe:
an organization or repository name does not prove which account should own it.
Keep read-only upstream remotes on HTTPS; if a repository contains remotes for
two different account aliases, diagnosis fails instead of choosing one by file
order.

Clone new repositories with the generic alias:

```sh
git clone git@github-account-1:OWNER/REPO.git
```

The remote then selects the account-specific commit identity and signing key
automatically. Direct `git@github.com` authentication is intentionally
fail-closed, while ordinary public HTTPS clones remain HTTPS. There is no global
`insteadOf` conversion.

Run GitHub CLI in the isolated account selected by the current repository:

```sh
git account gh -- pr status
git account gh -- repo view
```

Outside a matching repository, name the generic account explicitly:

```sh
git account gh account-2 -- auth status
```

This explicit command is intentional. Aliasing or shadowing `gh` would make
scripts depend on the current directory and would introduce surprising global
behavior.

## Diagnostics and real tests

Local checks do not make network requests:

```sh
git account doctor
```

The integration test authenticates with SSH, calls the GitHub API through the
isolated `gh` session, creates and verifies a temporary signed commit, and
performs a real shallow clone into a temporary directory:

```sh
git account test account-1 OWNER/REPO
```

Inside a migrated repository, both the account and `OWNER/REPO` can be inferred:

```sh
git account test
```

Before an isolated `gh` login exists, validate the remaining layers explicitly:

```sh
git account test --skip-gh account-2 OWNER/REPO
```

Unit tests for local rendering and account selection are available at:

```sh
shell/git/tests/git-account-test
```

## Local-only data

None of these paths is linked into or generated inside the dotfiles repository:

```text
~/.config/github-accounts/accounts/   account metadata
~/.config/github-accounts/keys/       comment-free public keys
~/.config/github-accounts/ssh/        generated SSH aliases
~/.config/github-accounts/git/        conditional Git identities and signing
~/.config/github-accounts/gh/         isolated GitHub CLI configuration
~/.config/git/local.gitconfig         other machine-local Git settings
~/.config/ssh/local.conf              other machine-local SSH hosts
```

Directories are mode `0700`; generated files are mode `0600`. Existing local
data is preserved when the Git or SSH topic is disabled.

## Recovery and history

`git account render` reconstructs derived SSH and conditional Git files from
the local account metadata. It is safe to run repeatedly. Back up
`~/.config/github-accounts/` through an encrypted local backup if account setup
must survive a machine loss; never copy it into this repository.

Removing PII or public keys from the current tree does not erase them from older
Git commits. This setup deliberately performs no history rewrite, force-push,
key upload, key deletion, or key revocation. Those are separate, explicit
operations with different risk.

## Troubleshooting

- **SSH reports a different login**: run `git account discover`, then update the
  account with the correct `--agent-index`. `test` compares GitHub's greeting to
  the locally recorded login and fails on a mismatch.
- **`gh` opens the wrong browser account**: choose “Use a different account” in
  GitHub's device flow and sign in to the login recorded for `account-N`.
  `login`, `import-gh`, `doctor`, and `test` verify that association.
- **Git reports multiple account aliases**: keep read-only upstream remotes on
  HTTPS. Two authenticated account aliases in one repository are ambiguous and
  intentionally fail diagnosis.
- **Signing works locally but GitHub does not show Verified**: add the public
  signing key to the same GitHub account with key type “Signing”. Authentication
  keys and signing keys are never uploaded automatically.
- **The agent key is unavailable**: unlock Bitwarden, enable its SSH agent, and
  confirm `SSH_AUTH_SOCK` points to the Bitwarden socket before running
  `doctor` again.
- **Generated files were edited or lost**: edit the authoritative local account
  with `git account add account-N`, then run `git account render`. Do not edit
  generated SSH or conditional include files by hand.
