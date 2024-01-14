#!/usr/bin/env bash
set -e

URL='https://openmath.org/cd/arith1#unary_minus'
SYM="<csymbol definitionURL=\"$URL\">unary_minus</csymbol>"

printf '<apply>%s%s</apply>' "$SYM" "$(cat)"
