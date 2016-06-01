#!/bin/bash 
export CCL=/usr/local/bin/ccl64
FILE="$(basename $1)"
NAME="${FILE%%.*}"
cl-launch -l ccl  --output "$NAME" -d '!' -L $1 --entry 'main'
chmod u+x "$NAME"
