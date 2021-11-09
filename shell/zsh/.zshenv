# Somehow, this environment variable isn't set in some envirnoment.
export SHELL=$(which zsh)

# Skip the not really helping Ubuntu global compinit. See the details:
# https://github.com/zdharma-continuum/zinit#disabling-system-wide-compinit-call-ubuntu
skip_global_compinit=1

# Prevent Zsh from loading ~/.profile every time.
if [ -z "$ARCHE_SKIP_PROFILE" ]; then
    emulate sh -c ". \$HOME/.profile"
fi
