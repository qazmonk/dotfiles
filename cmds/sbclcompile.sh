#!/bin/bash
PACKAGE="CL-USER"
if [ -n "$3" ]; then
    PACKAGE="$3"
fi
buildapp --eval "(load \"$1\")" --entry "$PACKAGE:main" --output $2
