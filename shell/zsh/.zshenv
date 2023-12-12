# See .zprofile for some explanation of what is going on here.

if [ -z "$ARCHE_SKIP_PROFILE" ]; then
    emulate sh -c '. "$HOME/.profile"'
fi
export ARCHE_SKIP_PROFILE=1

# Somehow, this environment variable isn't set in some envirnoment.
SHELL=$(which zsh)
export SHELL

# Skip the not really helping Ubuntu global compinit. See the details:
# https://github.com/zdharma-continuum/zinit#disabling-system-wide-compinit-call-ubuntu
skip_global_compinit=1
