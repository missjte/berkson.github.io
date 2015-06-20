#!/usr/bin/env bash

REMOTE="git@github.com:berkson/berkson.github.io.git"
DEPLOY="_site/"

info() {
  printf "  \033[00;32m+\033[0m $1\n"
}

success() {
  printf "  \033[00;32m+\033[0m $1\n"
}

fail() {
  printf "  \033[0;31m-\033[0m $1\n"
  exit
}

# shouldn't happen since `site` binary is usually at root to
# begin with, but doesn't hurt to check
dir_check() {
  if [ ! -f "blog.hs" ]; then
    fail "not at root dir"
  fi
}

git_check() {
  git rev-parse || fail "$PWD is already under git control"
}

setup() {
  dir_check

  rm -rf $DEPLOY
  mkdir $DEPLOY

  info "created $DEPLOY"
  cd $DEPLOY
  git_check

  git init -q
  info "initialized git"
  git checkout --orphan master -q
  info "established master branch"
  git remote add origin $REMOTE
  info "established git remote"

  success "setup complete"
}

deploy() {
  dir_check

  COMMIT=$(git log -1 HEAD --pretty=format:%H)
  SHA=${COMMIT:0:8}

  info "commencing deploy operation based off of $SHA"

  # clean out deploy & cache and move in the new files

  cd $DEPLOY
  find . -path ./.git -prune -o -exec rm -rf {} \; 2> /dev/null
  cd ..
  rm -rf _cache/

  info "cleaned out $DEPLOY & cache"
  info "building site"

  if [[ "$OSTYPE"x == "msys"x ]]; then
    # no unicode support in msys, so invoke powershell and establish code page
    powershell "chcp 65001; ./blog build" > /dev/null
  else
    ./blog build > /dev/null
  fi

  cd $DEPLOY

  git add --all .
  info "added files to git"

  git commit -m "generated from $SHA" -q
  info "committed site"

  git push origin master --force -q
  success "deployed site"
}

case "$1" in
  setup )
    setup;;
  deploy )
    deploy;;
  * )
    fail "invalid operation";;
  esac
