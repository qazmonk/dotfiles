########
# pyenv
########
# export PYENV_ROOT="$HOME/.pyenv"
# command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"


######
# pnpm
######
# export PNPM_HOME="/home/nate/.local/share/pnpm"
# export PATH="$PNPM_HOME:$PATH"
# pnpm end
#. "$HOME/.cargo/env"


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/nate/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/nate/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/nate/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/nate/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
