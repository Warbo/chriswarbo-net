#!/usr/bin/env bash
set -e

if [[ -n "$1" ]]
then
    NAME="$1"
else
    NAME=$(cat)
fi
printf '<ci>%s</ci>' "$NAME"
