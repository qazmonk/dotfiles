emacs-config : init.el
	ln -s ~/dotfiles/init.el ~/.emacs

bashrc: .bashrc.global .bashrc.base
	chmod a+x ~/dotfiles/.bashrc.global
	if [ -f ~/.bashrc ]; then mv ~/.bashrc ~/.bashrc.old; fi
	ln -s ~/dotfiles/.bashrc.base ~/.bashrc


