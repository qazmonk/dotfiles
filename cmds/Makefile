bash_scripts = $(wildcard *.sh)
python_scripts = $(wildcard *.py)


all : $(bash_scripts:.sh=) $(python_scripts:.py=)


% : %.sh
	ln -sf ~/dotfiles/cmds/$< ~/dotfiles/bin/$@
	chmod a+x ~/dotfiles/bin/$@

% : %.py
	ln -sf ~/dotfiles/cmds/$< ~/dotfiles/bin/$@
	chmod a+x ~/dotfiles/bin/$@



