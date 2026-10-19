[dotfiles](../README.md) › docs › troubleshooting

# Troubleshooting

## `git@github.com: Permission denied (publickey)`

Ordinary clones use the Bitwarden SSH agent, with no account profile. Run
`ssh-agent-check` to inspect the effective SSH config and the agent socket; it never
lists keys, unlocks the vault or contacts GitHub, so only a real clone proves access.

If several GitHub accounts need different keys, add a profile with `git account add`
and clone with `git account clone account-N OWNER/REPO`. See
[GitHub SSH profiles](../shell/git/README.md).

## Git asks for the GPG passphrase on every commit

Check that Git points at the right key:

```sh
gpg --list-secret-keys --keyid-format LONG
git config --global user.signingkey KEY_ID
```

More in GitHub's guide to
[telling Git about your GPG key](https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key).

## A topic misbehaves after an update

1. `dot -t TOPIC` shows what would run without changing anything.
2. `dot -f TOPIC` re-runs its install and link steps.
3. `dot -c` removes dead symlinks left by renamed or deleted files.

## Apple Silicon and Intel

The same topics work on both:

- **Homebrew** lives in `/opt/homebrew` on Apple Silicon and `/usr/local` on Intel;
  `.zshenv` picks the right one.
- **Apple Silicon-only apps** such as OrbStack are skipped on Intel.
- **`macosdefaults`** targets macOS 15 and later and closes System Settings, or System
  Preferences on older releases, before changing anything.
- **`SSH_AUTH_SOCK`** points at the Bitwarden agent only when its socket exists.
