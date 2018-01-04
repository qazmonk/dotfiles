#!/bin/bash

WIFI=$(iwconfig 2> /dev/null | sed -n 's/.*ESSID:"\([A-Za-z0-9\-]*\)"/\1/p' | tr -d '[:space:]')

case $WIFI in
    B3BFL|MillerWiLife-*)
	;;
    *)
	fusermount -ul ~/mnt
	;;
esac
