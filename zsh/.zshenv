########
# pyenv
########
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"


######
# pnpm
######
# export PNPM_HOME="/home/nate/.local/share/pnpm"
# export PATH="$PNPM_HOME:$PATH"
# pnpm end
