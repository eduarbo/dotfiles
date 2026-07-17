# My homies ese! 😎

A tidy `$HOME` is a tidy mind.


![Kitty and Emacs - Jul-2023 Screenshot of macOS Ventura](assets/kitty-emacs.png)

![Doom Emacs - Jul-2023 Screenshot of macOS Ventura](assets/doom-emacs.png)

These are my dotfiles, designed for macOS (Apple Silicon and Intel) and Debian 12, heavily inspired by
[hlissner/dotfiles](https://github.com/hlissner/dotfiles). Topics are split into `category/topic` (e.g. `shell/zsh`), aim for XDG compliance, and clutter your `$HOME` as little as possible.


## Dependencies
- [Homebrew](https://brew.sh/) (for macOS)
- git
- zsh
- curl


## Quick start

```sh
git clone --recurse-submodules https://github.com/eduarbo/dotfiles.git ~/.config/dotfiles
cd ~/.config/dotfiles

# On a fresh clone the `dot` alias doesn't exist yet, so invoke the script
# directly. Deploy the shell first — the `dot` alias is available afterwards.
./deploy shell/zsh
```

If you cloned without `--recurse-submodules`, fetch the Doom snippets submodule
with `git submodule update --init` before deploying `editor/emacs`.


## What does it include?

The following are the categories and topics you can install:

- `macos/` – Mac-specific tools and tweaks
  - `apps` – The essential macOS app lineup I can't live without
  - `defaults` – Opinionated values for a saner (and possibly sassier) macOS
  - `hammerspoon` – Lua-powered automation for pro-level productivity
    - Window wrangling without the wrestling
    - Lightning-fast app launcher
    - Push-to-talk mic sorcery (menubar toggle; hotkey opt-in via `bindings.lua`)
    - Instantly swap your audio outputs like a DJ
  - `karabiner` – The ultimate keyboard wizardry to remap all the things
  - `raycast` – Encrypted snapshot of my Raycast config, with export/import helpers

- `editor/` – My battle-stations for text and code
  - `emacs` – The best of both Emacs and Vim worlds, with extra chaos
  - `nvim` – My nimble sidekick for those “just one quick edit” moments
  - `coding-style` – Keep your code prettier than your neighbor's garden

- `shell/` – Terminal superpowers and creature comforts
  - `git` – Snazzy aliases and Zsh plugins for effortless versioning
  - `ssh` – SSH client config with Bitwarden agent support and a default identity
  - `tmux` – Tab-multiplying terminal wizardry
  - `zsh` – The shell with speed, features, and a prompt that actually sparks joy
  - `kitty` – The terminal so full-featured, even your cat would approve
  - `claude-code` – Custom status line for Claude Code CLI, integrated with Powerlevel10k

- `dev/` – Essential tools and setups for a life in code
  - `mise` – Universal version manager for Node, Python, Lua and more
  - `lua` – Lua toolchain (brew/apt), luarocks packages, and luacheck linter
  - `node` – npm/pnpm package management — runtime versions handled by mise
  - `python` – Python tooling via brew and pipx — runtime versions handled by mise
  - `golang` – Go fast, install Go, and grab some must-have packages
  - `php` – Composer support included, because sometimes you just can’t avoid PHP


## Dotfile management

The manager is the `deploy` script. On a fresh clone run it directly as
`./deploy`; once `shell/zsh` is enabled it's also available as the `dot` alias.

```
Usage: dot [-AacdfhlLit] [TOPIC...]

  -A   Target all available topics supported on this OS (shell bootstrap first)
  -a   Target all enabled topics (ignores TOPIC args)
  -c   Afterwards, remove dead symlinks & empty dot-directories in $HOME.
       Can be used alone.
  -d   Disable & unlink topic(s), runs `clean()`
  -f   Force install & link
  -l   Only relink topic(s) (implies -i)
  -L   List enabled topics
  -i   Inhibit install/update/clean init scripts
  -t   Do a test run; do not actually do anything

TOPIC can be an exact topic (`shell/zsh`), a group (`shell`), or a glob
(`shell/*`, `editor/n*`). Expanded groups and globs are filtered to topics that
support the current OS.
```

### Examples
+ `dot shell/zsh shell/kitty`: enables `shell/zsh` and `shell/kitty`
+ `dot shell`: enables all `shell/*` topics supported on this OS
+ `dot 'editor/*'`: enables all matching editor topics supported on this OS
+ `dot -A`: enables all available topics supported on this OS
+ `dot -d shell/zsh`: disables shell/zsh & cleans up after it
+ `dot -l shell/zsh`: refresh links for shell/zsh (inhibits init script)
+ `dot -f macos/karabiner`: force re-run install & link for an enabled topic
+ `dot -l`: relink all enabled topics
+ `dot -L`: list all enabled topics

Here's a breakdown of what the script does:

``` sh
cd $topic
if [[ -L $DOTFILES_DATA/${topic/\//.}.topic ]]; then
    ./_init update
else
    ln -sfv $DOTFILES/$topic $DOTFILES_DATA/${topic/\//.}.topic

    ./_init install
    ./_init link
fi
```

## Best practices

- If you use a password manager like Bitwarden, managing SSH keys is a breeze (and much more convenient). But if you're old-school (or just like a little DIY), you can still generate SSH keys manually:

  ```sh
  ssh-keygen -t ed25519 -C "personal-mbpro-2025" # Use a descriptive comment: purpose + device + year
  ```

### Managing Forge/GitHub Tokens

Forge (Magit’s helper for GitHub/GitLab) reads your personal access token from the file set in `auth-sources`. This config points it at `~/.config/doom/authinfo.gpg` (see `editor/emacs/doom/+defaults.el`). This file should never be committed to your repository.

To set up your tokens securely:
- This repo provides a template at `editor/emacs/doom/authinfo.gpg.example`. Copy it to `editor/emacs/doom/authinfo.gpg` (same directory)
- Run `dot -l editor/emacs` to relink the `doom/` directory (the file then appears at `~/.config/doom/authinfo.gpg`)
- Open and edit the file in Emacs to add your tokens, then save. Emacs will manage the GPG encryption for you

By following these steps, your dotfiles remain clean, secure, and portable—reducing the risk of accidentally exposing your credentials.

## Intel vs Apple Silicon

These dotfiles work on both Intel and Apple Silicon Macs. A few things to be aware of:

- **Homebrew paths**: Homebrew installs to `/opt/homebrew` on Apple Silicon and `/usr/local` on Intel. The shell config auto-detects the correct path.
- **ARM-only apps**: Some casks like OrbStack are Apple Silicon only and will be skipped automatically on Intel Macs.
- **macOS defaults**: The `macos/defaults` script detects the macOS version to handle differences between System Preferences (pre-Ventura) and System Settings (Ventura+).
- **SSH agent**: If using Bitwarden's SSH agent, the `SSH_AUTH_SOCK` is only set when the socket file exists.
- **Gitea SSH**: New clone URLs use `git@ssh.guts.cc:...` and are routed through Cloudflare Access. Existing `git@guts.cc:...` remotes remain compatible. The `shell/ssh` topic installs `cloudflared`, links the public half of the personal key used by Bitwarden, and configures both hostnames. If Keychain contains `cloudflare-ssh-polyhunter-client-id` and `cloudflare-ssh-polyhunter-client-secret`, the proxy uses the dedicated service token without browser SSO; otherwise the normal interactive Access login remains available. Test the full path with `ssh -T git@ssh.guts.cc`.


## Troubleshooting

### Signing Git commit with GPG ask passphrase everytime

Make sure the GPG key ID is correct. You can get a list of GPG keys with
`gpg --list-secret-keys --keyid-format LONG` and then set it in Git with
`git config --global user.signingkey [GPG-key-ID]`.

More details in [Telling Git about your GPG key](https://help.github.com/articles/telling-git-about-your-gpg-key/).

### git@github.com: Permission denied (publickey)

Just [add your SSH Key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account) to your GitHub account


## Relevant projects/resources

+ [DOOM Emacs](https://github.com/doomemacs/doomemacs) (pulled by `editor/emacs`)
+ [Pacmux Tmux theme](https://github.com/eduarbo/pacmux) (pulled by `shell/tmux`)
+ [Powerlevel10k](https://github.com/romkatv/powerlevel10k) (zsh prompt, loaded via zinit in `shell/zsh`)
