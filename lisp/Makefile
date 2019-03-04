lisp_files = $(wildcard *.lisp)

all : $(lisp_files:.lisp=)

% : %.lisp
	buildapp --output ~/dotfiles/bin/$@ --load $< --entry main
