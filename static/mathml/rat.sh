#!/usr/bin/env bash
set -e

printf '<apply><csymbol cd="nums1">rational</csymbol>'
if [[ -n "$1" ]] && [[ -n "$2" ]]
then
    printf '%s%s' "$1" "$2"
else
    cat
fi
printf '</apply>'
