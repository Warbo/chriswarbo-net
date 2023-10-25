#!/usr/bin/env bash
set -eu

BUCKET='www.chriswarbo.net'

SITE=''
if [[ "$#" -eq 1 ]]
then
    echo "Using '$1' as site root" 1>&2
    SITE="$1"
else
    if [[ -e result ]]
    then
        SITE='result'
    else
        {
            echo "Did not find a site root to push: aborting."
            echo "Either provide it as an argument, or nix-build a 'result'"
            exit 1
        } 1>&2
    fi
fi
echo "Using '$SITE' as site root" 1>&2
for EXPECTED in index.html projects blog blog.html js css
do
    [[ -e "$SITE/$EXPECTED" ]] || {
        echo "Given site '$SITE' does not contain '$EXPECTED', aborting"
        exit 1
    } 1>&2
done

uploadPages() {
    local args
    # These speed up syncing with S3 (fewer roundtrips, etc.)
    args=(--fast-list --no-update-modtime --checksum)

    # Exclude the /git folder from the sync: without this, sync will try to
    # delete all of the /git/foo folders that exist in S3.
    args+=(--filter '- /git/')

    # Our Nix result is full of symlinks, which must be dereferenced
    args+=(-L)

    # Default to dry-run, unless DO_PUSH=1
    [[ "${DO_PUSH:-0}" -eq 1 ]] || args+=(--dry-run)

    RCLONE_S3_REGION=eu-west-1 RCLONE_S3_PROVIDER=AWS  RCLONE_S3_ENV_AUTH=true \
        with-aws-creds \
          rclone sync "${args[@]}" "$SITE" ":s3:$BUCKET"
}

setGitRedirect() {
    # Temporary workaround (as of 2023-07-03...) to redirect chriswarbo.net/git
    # requests to github.com/warbo. We want /git/foo.git to be redirected, but
    # not /git/foo (the human-readable HTML interfaces): to achieve that effect,
    # we just redirect all 404s, so existing objects (like /git/foo) will be
    # served, but non-existing objects (like /git/foo.git, among others) will be
    # redirected.
    # NOTE: We don't use S3's object-based redirects (which use metadata), since
    # those emit 301 PERMANENT redirects, and we want 302 TEMPORARY redirects.
    CONFIG='{
        "IndexDocument": {
            "Suffix": "index.html"
        },
        "RoutingRules": [
            {
                "Condition": {
                    "KeyPrefixEquals": "git/",
                    "HttpErrorCodeReturnedEquals": "404"
                },
                "Redirect": {
                    "HostName": "github.com",
                    "ReplaceKeyPrefixWith": "warbo/"
                }
            }
        ]
    }'

    with-aws-creds aws s3api put-bucket-website \
        --bucket "$BUCKET" \
        --website-configuration "$CONFIG"
}

uploadPages
setGitRedirect
