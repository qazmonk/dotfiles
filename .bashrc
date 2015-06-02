export BASH_CONF="bashrc"

alias ls="ls -G"
alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -a \"\""
alias sshpclassic="ssh pclassic@pclassic.org"
alias ssheniac="ssh nchodosh@eniac.seas.upenn.edu"
alias e="emacs"
alias calc="~/dotfiles/cmds/calc.sh"
set -o emacs

#Configure opam
eval `opam config env`
