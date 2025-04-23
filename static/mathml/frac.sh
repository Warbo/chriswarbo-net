#!/usr/bin/env bash
set -e

# Takes two raw numbers as arguments and returns outputs a rational number
# See also rat.sh

printf '<cn type="rational">%s<sep/>%s</cn>' "$1" "$2"
