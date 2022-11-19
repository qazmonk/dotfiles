#!/bin/bash

systemctl --user is-active --quiet unison
status=$?
if [ $status -eq 0 ]
then
    last_log=$(journalctl --user-unit=unison --no-pager -n 300 | ~/dotfiles/bin/unison-last-sync)
    if [[ $last_log = Fatal* ]]
    then
	echo "RUNNING: NO CONNECTION"
	echo "NO CONNECT"
	echo "#Fce705"
    else
	echo "RUNNING: $last_log"
	echo "SYNC"
	echo "#11FF80"	
    fi
else
    echo "NOT RUNNING"
    echo "OFF"
    echo "#FF0808"
fi

