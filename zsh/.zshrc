#Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
#Initialization code that may require console input (password prompts, [y/n]
#confirmations, etc.) must go above this block; everything else may go below.
#if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
#fi

# Use powerline
# USE_POWERLINE="true"
# # Source manjaro-zsh-configuration
# if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
#   source /usr/share/zsh/manjaro-zsh-config
# fi
# # Use manjaro zsh prompt
# if [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
#   source /usr/share/zsh/manjaro-zsh-prompt
# fi
# Detect org-babel/emacs sessions and skip complex setup
if [[ "$INSIDE_EMACS" = *comint* ]] || [[ "$TERM" = "dumb" ]]; then
    # Disable P10k configuration wizard warning
    export POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true
    
    # Load machine-specific environment
    source ~/.zshenv.this-machine
    
    export CONDA_CHANGEPS1=false
    # Keep conda initialization
    __conda_setup="$('/home/nchodosh/miniforge3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/home/nchodosh/miniforge3/etc/profile.d/conda.sh" ]; then
            . "/home/nchodosh/miniforge3/etc/profile.d/conda.sh"
        else
            export PATH="/home/nchodosh/miniforge3/bin:$PATH"
        fi
    fi
    unset __conda_setup
    
    # Keep essential settings
    autoload -U compinit; compinit
    autoload -U select-word-style
    select-word-style bash
    alias ls="ls -lah"
    
    # Basic history settings
    export HISTFILE=~/.history
    export HISTSIZE=5000000
    export SAVEHIST=$HISTSIZE
    setopt SHARE_HISTORY
    setopt HIST_IGNORE_DUPS
    
    # Let org-babel handle the prompt - don't fight it
    # Skip the rest of the complex setup
    return 0
fi


source ~/.zshenv.this-machine
# use p10k
#source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
source $P10K_THEME_FILE #/home/nchodosh/github-src/powerlevel10k/powerlevel10k.zsh-theme

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

prompt_vterm_dir() {
     vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

# Convert video to gif file.
# Usage: video2gif video_file (scale) (fps)
mp42gif() {
  ffmpeg -y -i "${1}" -vf fps=${3:-10},scale=${2:-320}:-1:flags=lanczos,palettegen "${1}.png"
  ffmpeg -i "${1}" -i "${1}.png" -filter_complex "fps=${3:-10},scale=${2:-320}:-1:flags=lanczos[x];[x][1:v]paletteuse" "${1%.mp4}".gif
  rm "${1}.png"
}
# setopt PROMPT_SUBST
# PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
# Convert video to gif file.
# Usage: video2gif video_file (scale) (fps)
mp42gif() {
  ffmpeg -y -i "${1}" -vf fps=${3:-10},scale=${2:-320}:-1:flags=lanczos,palettegen "${1}.png"
  ffmpeg -i "${1}" -i "${1}.png" -filter_complex "fps=${3:-10},scale=${2:-320}:-1:flags=lanczos[x];[x][1:v]paletteuse" "${1%.mp4}".gif
  rm "${1}.png"
}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


export HISTFILE=~/.history
export HISTSIZE=5000000
export SAVEHIST=$HISTSIZE

# HISTORY
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt SHARE_HISTORY             # Share history between all sessions.
# END HISTORY


autoload -U compinit; compinit
autoload -U select-word-style
select-word-style bash

# Enable colors for ls
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# Colorized ls aliases
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux (GNU coreutils)
    alias ls='ls --color=auto'
    alias ll='ls --color=auto -lah'
    alias la='ls --color=auto -la'
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    alias ls='ls -G -lah'
    alias ll='ls -G -l'
    alias la='ls -G -la'
else
    # Fallback
    alias ls='ls -lah'
fi

# Load vterm utility functions
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi


# pyenv

# export PYENV_ROOT="$HOME/.pyenv"
# [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"



