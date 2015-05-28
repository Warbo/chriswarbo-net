#!/bin/sh

# Fetch git repos from chriswarbo.net

# Allow source to be overridden
if [ -z "$SOURCE" ]
then
    SOURCE="http://chriswarbo.net/git"
fi

# Allow ".git" suffix to change for sources
if [ -z "$SUFFIX" ]
then
    SUFFIX=".git"
fi

# All repos
repos=(ant-colony apt-repo-tools arduino-json-client arrowlets-for-nodejs
       bitbitjump-maude chriswarbo-net chrome-duplicate-tab-detector
       clutter-file-browser debian-repo-packager dependent-types-talk
       genetic-turing-machines gnucleon gnucleon-clutter gpu-simulations iddish
       iron java-spider javascript-base64 js-plumb lazy-lambda-calculus
       mc-aixi-ctw ml4hs ml4pg ocportal-salmon panpipe panhandle
       particle-storage-ring-simulation php-prelude php-core php-easycheck
       php-plumb powerplay pubsubclient purely-functional-self-modifying-code
       python-clutter-experiments python-decompiler python-pipes-simulation
       python-plumb search-optimisation-streams service-packs tree-features
       turtleviewer warbo-dotfiles warbo-utilities)

for repo in "${repos[@]}"
do
    # Bare clone the repo
    target="${repo}.git"
    git clone --bare "${SOURCE}/${repo}${SUFFIX}" "${target}"

    # Enable post-update hook to accept pushes
    mv "${target}/hooks/post-update.sample" "${target}/hooks/post-update"
    chmod a+x "${target}/hooks/post-update"

    # Run post-update hook to initialise
    (cd "${target}" && sh hooks/post-update)
done
