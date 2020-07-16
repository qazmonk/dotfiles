#!/bin/bash



while read line
do
  echo $line 
done < "${1:-/dev/stdin}" | tac | sed -E -n 's/([a-zA-Z 0-9]{7}[0-9]{2}:[0-9]{2}:[0-9]{2}).*((Fatal .*) Lost connection.*|(Failed.*)|Synchronization (complete).*|(Looking).*|(Nothing).*)/\3\4 \1/p' | head -n 1
