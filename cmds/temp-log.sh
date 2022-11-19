#! /bin/bash

journalctl -u sensord --no-pager --grep CPUTIN -S "$1" | awk '{print $1 "-"  $2 "-" $3 " " $7}' > /tmp/temps.txt

