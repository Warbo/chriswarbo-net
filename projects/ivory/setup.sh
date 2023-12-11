#!/usr/bin/env bash
set -e

FILENAME="$1"
[[ -z "$FILENAME" ]] && {
    echo "setup.sh: need argument for .rkt filename"
    exit 1
} 1>&2

SHEBANG='#!'"$(command -v bash)"

# Commands to use in our pipe attributes: both append code to the end of
# $FILENAME # but one also shows it on the page and the other doesn't.
printf '%s\nexport LANG=en_US.UTF-8\ntee -a %s\necho >> %s\necho >> %s\n' \
       "$SHEBANG" "$FILENAME" "$FILENAME" "$FILENAME" > show

printf '%s\nexport LANG=en_US.UTF-8\ncat >> %s\necho >> %s\necho >> %s\n' \
       "$SHEBANG" "$FILENAME" "$FILENAME" "$FILENAME" > hide

# Runs a test suite using Raco; prints out the code listing on failure (so we
# can cross-reference line numbers from backtraces)
{
    printf '%s\nexport LANG=en_US.UTF-8\nraco test %s ' \
           "$SHEBANG" "$FILENAME"
    printf '|| {\necho %s\ncat -n %s\necho %s\nfalse\n} 1>&2' \
           "BEGIN $FILENAME" "$FILENAME" "END $FILENAME"
} > tests

# Extracts a Base64-encoded file from a rendered Ivory page. This allows pages
# to depend on the code from each other during rendering.
{
    printf '%s\nset -e\necho "Extracting code from $1" 1>&2\n' "$SHEBANG"
    printf "grep -o '%s' < %s > RAW\n" \
           "<a[^>]*id=\"racket\"[^>]*>" '"$1"'
    printf 'F=$(< RAW sed -e %s)\n' \
           "'s@.*download=\"\([^\"]*\)\".*@\1@g'"
    printf '< RAW sed -e "%s" -e "%s" | base64 -d > "$F"\n' \
           's/.*base64,//g' 's/[\"'\''].*$//g'
} > extract

chmod +x extract hide show tests
