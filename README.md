# 🏗 WIP - My homies ese!

> This a fresh start for my dotfiles, it's still a **Work in Progress**

A tidy `$HOME` is a tidy mind.

![Neofetch - Aug-2018 Screenshot of macOS High Sierra](assets/neofetch.png)

![Workflow - Aug-2018 Screenshot of macOS High Sierra](assets/workflow.png)

These are my dotfiles, designed primarily for macOS and Ubuntu, heavily inspired
by [hlissner/dotfiles](https://github.com/hlissner/dotfiles). They are my
specific breed of madness, split into 2-level topics (e.g. `shell/zsh`) that
strive for minimum `$HOME` presence (adhering to XDG standards where possible).

## Quick start

`bash <(curl -s https://raw.githubusercontent.com/eduarbo/homies/master/bootstrap.sh)`

## What will it include?

The following are the categories and topics available to install:

- `macos/` - macOS compatible utilities
  - [X] `apps` - collection of macOS apps that I use on a daily basis
  - [X] `defaults` - bare essentials for macOS
  - [X] `hammerspoon` - a bunch of lua scripts for macOS to boost my productivity
  - [X] `iterm` - iTerm2, is there a better macOS terminal?
  - [X] `karabiner` - powerful and stable keyboard customizer for macOS with
        useful helpers
  - [X] `kitty` - well, in fact there is a better macOS terminal

- `misc/` - Very handy apps & tools
  - [X] `surfingkeys` - vim-like bindings for Google Chrome
  - [ ] `tridactyl` - vim-like bindings for Firefox

- `editor/` - Configuration for my text editors
  - [X] `editorconfig` - maintain consistent coding styles between different editors
  - [X] `emacs` - the best of Emacs and Vim in one place
  - [X] `vim` - for quick and remote file editing

- `shell/` - Shell utilities
  - [X] `bash` - not my default shell but doesn't hurt to have a basic config
  - [X] `git` - nice aliases and zsh plugins
  - [ ] `gnupg` - encryption/signing
  - [ ] `sk` - faster than fzf, Skim it's Fuzzy Finder in rust!
  - [X] `tmux` - window manager within the terminal + nice plugins
  - [X] `zsh` - my shell of choice with a nice prompt, really fast and extensible with zplugin

- `dev/` - Relevant to software development & programming in general
  - [X] `lua` - manage lua environments with luaenv
  - [X] `node` - setup nodenv, a lightweight alternative to nvm
  - [X] `python` - setup pyenv, a simple Python version management


## Dotfile management

```
Usage: deploy [-acdlLit] [TOPIC...]

  -a   Target all enabled topics (ignores TOPIC args)
  -c   Afterwards, remove dead symlinks & empty dot-directories in $HOME.
       Can be used alone.
  -d   Unlink and run `./_init clean` for topic(s)
  -l   Only relink topic(s) (implies -i)
  -L   List enabled topics
  -i   Inhibit install/update/clean init scripts
  -t   Do a test run; do not actually do anything
```

e.g.
+ `deploy misc/kitty shell/{zsh,tmux}`: enables misc/kitty, shell/zsh & shell/tmux
+ `deploy -d shell/zsh`: disables shell/zsh & cleans up after it
+ `deploy -l shell/zsh`: refresh links for shell/zsh (inhibits init script)
+ `deploy -l`: relink all enabled topics
+ `deploy -L`: list all enabled topics

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

## Troubleshooting

### Signing Git commit with GPG ask passphrase everytime

Make sure the GPG key ID is correct. You can get a list of GPG keys with
`gpg --list-secret-keys --keyid-format LONG` and then set it in Git with
`git config --global user.signingkey [GPG-key-ID]`.

More details in [Telling Git about your GPG key](https://help.github.com/articles/telling-git-about-your-gpg-key/).

## Relevant projects/resources

+ [Wallpapers](https://drive.google.com/drive/folders/1FRy0ZOvau2A1Rp7hU8GE0dM8O_cIKhf-) (pulled by `shell/zsh`)
+ [DOOM Emacs](https://github.com/hlissner/doom-emacs) (pulled by `editor/emacs`)
+ [Pacmux Tmux theme](https://github.com/eduarbo/pacmux) (pulled by `shell/tmux`)
+ [Simpl ZSH prompt](https://github.com/eduarbo/simpl) (pulled by `shell/zsh`)
