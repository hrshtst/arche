#!/usr/bin/env zsh

### pipx

# The pipx provides a way to execute binaries from Python packages in
# isolated environments.
# https://github.com/pipxproject/pipx

# Autocompletion function for pipx is provided along with argcomplete,
# which is a dependency of pipx.
if (( $+commands[pipx] )); then
  autoload -Uz bashcompinit && bashcompinit
  eval "$(register-python-argcomplete pipx)"
fi

### pyenv
# The pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
if (( $+commands[pyenv] )); then
  eval "$(pyenv init -)"
fi

### PDM
# PDM is a project management tool.
# https://github.com/pdm-project/pdm
if (( $+commands[pdm] )); then
  eval "$(pdm --pep582 zsh)"
fi
