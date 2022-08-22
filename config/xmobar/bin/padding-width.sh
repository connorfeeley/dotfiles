#!/bin/bash

pname=${1:-panel}
width=$(xprop -name $pname | grep 'program specified minimum size' | cut -d ' ' -f 5)

echo "<hspace=${width:-0}/>"
