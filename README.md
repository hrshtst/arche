# Arche

**Arche** is a collection of dotfiles for my personal use. Many ideas
and implementations are incorporated and highly customized from Radon
Rosborough's [Radian](https://github.com/raxod502/radian).

## Installation

Installation process is devided into three parts: installing
prerequisite software, installing the configuration, and optionally
installing the local configuration.

### Installing prerequisite software

#### Ubuntu

- Emacs:
  - Language Server
    - Bash: `yarn global add bash-language-server`
    - C/C++: `apt install libclang-dev`
    - Go: `go install golang.org/x/tools/gopls@latest`
    - HTML: `yarn global add vscode-html-languageserver-bin`
    - JavaScript: `yarn global add typescript typescript-language-server`
    - LaTeX: `wget --directory-prefix ~/.local/bin https://raw.githubusercontent.com/astoff/digestif/master/scripts/digestif`
    - Python: `pipx install pyright`

### Installing configuration

Use symbolic links:

``` shell
make link
```

### Installing local configuration

Go to [my private repository](https://github.com/hrshtst/arche.local)
