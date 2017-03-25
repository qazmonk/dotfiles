export BASH_CONF="bashrc"


LOCAL_BASHRC=".bashrc.local"
if [ -e "$LOCAL_BASHRC" ]
then
   source $LOCAL_BASHRC
fi

export PATH=/usr/local/bin:$PATH
export PATH=$PATH:/Applications/TeX/TeXShop.app/Contents/Resources/TeXShop/bin/tslatexmk
export PATH=$PATH:/Applications/MATLAB_R2015b.app/bin
alias start-win-emacs-server="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon=win"
alias start-nw-emacs-server="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon=nwin"
alias ls='ls -GFh'
alias ll='ls -l'
alias eclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -s win -c"
alias eclientnw="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -s nwin -c -nw"
alias sshpclassic="ssh pclassic@pclassic.org"
alias ssheniac="ssh nchodosh@eniac.seas.upenn.edu"
alias ssh-seedbox="ssh qazmonk@burrito.whatbox.ca"
alias e="eclient"
alias kill-win-emacs-server="eclient -e \"(kill-emacs)\""
alias kill-nw-emacs-server="eclientnw -e \"(kill-emacs)\""
alias calc="~/dotfiles/cmds/calc.sh"
set -o emacs
export EDITOR="eclient"
#Configure opam
eval `opam config env`

export NVM_DIR="/Users/Nate/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced


function prompt {
    local BLACK="\[\033[0;30m\]"
    local BLACKBOLD="\[\033[1;30m\]"
    local RED="\[\033[0;31m\]"
    local REDBOLD="\[\033[1;31m\]"
    local GREEN="\[\033[0;32m\]"
    local GREENBOLD="\[\033[1;32m\]"
    local YELLOW="\[\033[0;33m\]"
    local YELLOWBOLD="\[\033[1;33m\]"
    local BLUE="\[\033[0;34m\]"
    local BLUEBOLD="\[\033[1;34m\]"
    local PURPLE="\[\033[0;35m\]"
    local PURPLEBOLD="\[\033[1;35m\]"
    local CYAN="\[\033[0;36m\]"
    local CYANBOLD="\[\033[1;36m\]"
    local WHITE="\[\033[0;37m\]"
    local WHITEBOLD="\[\033[1;37m\]"
    local RESETCOLOR="\[\e[00m\]"

    export PS1="\n$RED\u $PURPLE@ $GREEN\w $RESETCOLOR$GREENBOLD\n\$(git branch 2> /dev/null)\n $BLUE[\#] → $RESETCOLOR"
    export PS2=" | → $RESETCOLOR"
}

case "$TERM" in
    "dumb")
        PS1="$ "
        ;;
    xterm*)
        prompt
        ;;
esac

