#!/bin/bash

[[ -z $1 ]] && echo "Usage: toggle-pipe.sh path-to-pipe" && exit 1

state="$1.state"

[[ -p $1 ]] || mkfifo $1

[[ -f $state ]] || echo 0 > $state

v=$(<$state)

if [[ x$v == x1 ]]; then v=0; else v=1; fi

echo $v > $state;
echo -e "$v\\\n" > $1;
