#!/bin/bash

status=$(ps -ae -o cmd | grep ^IDrive:service-watcher)
if [ $status == "" ]
then
    echo "NO BACKUPS"
    echo "OFF"
    echo "#FF0808"
else
    echo "BACKUP RUNNING"
    echo "SYNC"
    echo "#11FF80"
fi
   
   


