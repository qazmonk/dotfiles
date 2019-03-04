#!/bin/bash



tail $1 | tac | sed -E -n 's/UNISON.*finished prop.*([0-9]{2}:[0-9]{2}:[0-9]{2}).*on (.*$)/\1 \2/p'
