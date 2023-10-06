#!/usr/bin/env bash
mkdir home
export HOME="$PWD/home"
git config --global user.email "you@example.com"
git config --global user.name "Your Name"

mkdir html
mkdir test.git
pushd test.git
  git init
  SUBJECT="Need a foo" BODY="For testing" git artemis add
  echo "Testing" > foo
  git add foo
  git commit -m "Added foo"

  SUBJECT="Need a bar" BODY="More testing" git artemis add
  echo "Testing"     >> foo
  echo "123"         >  bar
  echo "$testReadme" >  README.md
  git add foo bar README.md
  git commit -m "Added bar and README"
popd

if "$script"
then
  fail "Should reject when no args"
fi

if "$script" test.git
then
  fail "Should reject when one arg"
fi

"$script" test.git html

if [[ -e test.git/repository ]]
then
  fail "Left over repo should have been deleted"
fi

[[ -e html/index.html          ]] || fail "No index.html"
[[ -e html/git/index.html      ]] || fail "No git/index.html"
[[ -e html/issues/threads.html ]] || fail "No issues/threads.html"

mv html "$out"
