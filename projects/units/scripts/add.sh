#!/usr/bin/env bash
set -e

URL='https://openmath.org/cd/arith1#plus'
SYM="<csymbol definitionURL=\"$URL\">plus</csymbol>"

printf '<apply>%s%s</apply>' "$SYM" "$(cat)"
