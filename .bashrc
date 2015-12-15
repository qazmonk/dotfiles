export BASH_CONF="bashrc"

export PATH=$PATH:/Applications/TeX/TeXShop.app/Contents/Resources/TeXShop/bin/tslatexmk
export PATH=$PATH:/Applications/MATLAB_R2014a.app/bin/
alias start-emacs-server="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"
alias ls="ls -G"
alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -a \"\""
alias sshpclassic="ssh pclassic@pclassic.org"
alias ssheniac="ssh nchodosh@eniac.seas.upenn.edu"
alias ssh-seedbox="ssh qazmonk@burrito.whatbox.ca"
alias e="emacs"
alias kill-emacs-server="e -e \"(kill-emacs)\""
alias calc="~/dotfiles/cmds/calc.sh"
set -o emacs

#Configure opam
eval `opam config env`
