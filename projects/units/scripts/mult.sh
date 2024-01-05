#!/usr/bin/env bash
set -e

URL='https://openmath.org/cd/arith1#times'
SYM="<csymbol definitionURL=\"$URL\">times</csymbol>"

printf '<apply>%s%s</apply>' "$SYM" "$(cat)"
