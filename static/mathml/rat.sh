#!/usr/bin/env bash
set -e

# Applies 'rational' to two elements (either given as arguments, or piped in).
# See also frac.sh

printf '<apply><csymbol cd="nums1">rational</csymbol>'
if [[ -n "$1" ]] && [[ -n "$2" ]]
then
    printf '%s%s' "$1" "$2"
else
    cat
fi
printf '</apply>'
