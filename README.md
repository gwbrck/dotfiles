# Dotfiles

This repository contains my personal dotfiles and configurations, managed using [chezmoi](https://chezmoi.io/). Chezmoi allows for the handling of dotfiles across several machines, and offers capabilities for distinguishing various operating systems, package managers, amongst numerous other features.

## Getting Started on a New Machine

To deploy these dotfiles on a new machine, follow the steps below:

1. Install `chezmoi`, and initialize it with my Github user as shown:
    ```sh 
    sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply gwbrck
    ```
    This command runs the `chezmoi` setup script fetched from the official `chezmoi` website. Using the `--apply` option, `chezmoi` applies the changes immediately after initialization.

2. After setting up `chezmoi`, the next steps involve correctly transferring your GPG keys to the new machine and setting up your SSH connection to your GitHub account.

3. Lastly, change the `chezmoi` dotfiles repository remote from HTTPS to SSH:
    ```sh
    cd ~/.local/share/chezmoi
    git remote set-url origin git@github.com:gwbrck/dotfiles.git
    ```

## Development Environments

On macOS, `chezmoi apply` installs Nix and devenv. Subsequent applies update
the global devenv CLI with `nix profile upgrade devenv`. Start a new Fish
session after the initial bootstrap so that the Nix environment and devenv
hook are loaded:

```sh
exec fish
```

Initialize an R project:

```sh
mkdir analysis && cd analysis
rinit
devenv allow
devenv shell # optional; allowed projects activate automatically
```

Initialize a Python project. `uv init` must run before the first devenv shell
because the template enables automatic `uv sync`:

```sh
mkdir analysis && cd analysis
pyinit
uv init
devenv allow
devenv shell # optional
```

Update a project's pinned inputs from inside that project with:

```sh
devenv update
```

## Manual Steps

There are a few steps that need to be done manually after the automated setup:

### Zotero Extensions

Sign in to the Zotero account and install the following extensions:

1. [Better BibTeX](https://github.com/retorquere/zotero-better-bibtex)

2. [MarkDB-Connect](https://github.com/daeh/zotero-markdb-connect)
