#!/bin/bash

workspace_num=$(i3-msg -t get_workspaces | jq '.[] | select(.focused==true).name' | cut -d"\"" -f2)
daemon=emacs$workspace_num
emacs_socket=/run/user/1000/emacs/$daemon

if [ -e $emacs_socket ]
then
    emacsclient -s $daemon -c -e "(vterm 'new)" 
else
    emacs --daemon=$daemon
    emacsclient -s $daemon -c -e "(vterm 'new)" 
fi
