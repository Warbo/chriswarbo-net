#!/usr/bin/env bash
set -e
set -o pipefail

# Capture and discard stdin, to avoid pandoc waiting on user input
[ -t 0 ] || cat > /dev/null

OUT_OPTIONS=(-f json -t html -s --mathml --template "$defaultTemplate")

# Provide 'path' arg for our View Source links.
OUT_OPTIONS+=(-M "path=${SOURCE_PATH:-$SOURCE}")

# If filename contains a date (e.g. blogs), provide it to the output template
if D=$(echo "$SOURCE" | grep -om1 -P "\\d{4}-\\d{2}-\\d{2}")
then
    OUT_OPTIONS+=(-M "date=$D")
fi

# We don't use pandoc's --filter option, as it buffers stderr which makes
# debugging harder. 'postprocessor' comes from the file's YAML (default: 'cat')
pandoc -t json "$SOURCE"       |
    panpipe                    |
    panhandle                  |
    pandoc "${OUT_OPTIONS[@]}" |
    cleanup                    |
    "${postprocessor:-cat}"    > "$DEST" || fail "Rendering pipeline failed"

# Check output

[[ -e "$DEST" ]]                      || fail "Error: '$DEST' wasn't created"
grep '[^ \n\t]' < "$DEST" > /dev/null || fail "Error: Output is empty"
