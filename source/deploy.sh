#!/usr/bin/env bash

REMOTE="git@github.com:berkson/berkson.github.io.git"
SITE="generated/deploy/"
DEPLOY="deploy/"

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
vps_install() {
  if [ ! -f "vps.sh" ]; then
    cd ..
    ./source/vps.sh
  fi
}

git_check() {
  git rev-parse || fail "$PWD is already under git control"
}

setup() {
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

  vps_install

  success "setup complete"
}

deploy() {

  COMMIT=$(git log -1 HEAD --pretty=format:%H)
  SHA=${COMMIT:0:8}

  info "commencing deploy operation based off of $SHA"

  # clean out deploy & cache and move in the new files

  cd $DEPLOY
  find . -path ./.git -prune -o -exec rm -rf {} \; > /dev/null
  cd ..

  info "cleaned out $DEPLOY"
  info "building site"

  if [[ "$OSTYPE"x == "msys"x ]]; then
    # no unicode support in msys, so invoke powershell and establish code page
    powershell "chcp 65001; ./blog build" > /dev/null
  else
    ./blog build > /dev/null
  fi

  cp -r $SITE $DEPLOY
  info "copied $SITE into $DEPLOY"

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
