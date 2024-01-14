#!/usr/bin/env bash
set -e

if [[ -n "$1" ]]
then
    NUM="$1"
else
    NUM=$(cat)
fi
printf '<cn>%s</cn>' "$NUM"
