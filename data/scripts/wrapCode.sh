#!/bin/sh
function drawTicks() {
  COUNT=$(($1 + 3))
  while [ "$COUNT" -gt 0 ]
  do
    echo -n '`'
    COUNT=$(($COUNT - 1))
  done
}

BODY=$(cat)
PAT='`[`]*'
TICKS=$(echo "$BODY" | grep -o "$PAT" | sort | tail -n 1 | wc -c)

drawTicks "$TICKS"
echo -n '{'
echo -n "$@"
echo '}'

echo "$BODY"

drawTicks "$TICKS"
echo ""
