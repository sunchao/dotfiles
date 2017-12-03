#!/bin/bash

repos=(
  altercation/vim-colors-solarized
  itchyny/lightline.vim
  junegunn/fzf.vim
  mileszs/ack.vim
  tomasr/molokai
  tpope/vim-pathogen
  tpope/vim-fugitive
  scrooloose/nerdtree
)

set -e
DIR=$(cd $(dirname $0); pwd -P)/bundle
echo "DIR: $DIR"

if [ -d $DIR -a -z "$1" ]; then
  temp=$(mktemp -d -t bundleXXXXX)
  echo "Moving old bundle dir to $temp"
  mv "$DIR" "$temp"
fi
mkdir $DIR

for repo in ${repos[@]}; do
  if [ -n "$1" ]; then
    if ! (echo $repo | grep -i $i &> /dev/null); then
      continue
    fi
  fi
  plugin=$(basename $repo | sed -e 's/\.git$//')
  dst="$DIR/$plugin"
  rm -rf $dst
  (
    git clone --depth=1 -q https://github.com/$repo $dst
    rm -rf $dst/.git
    echo "Cloned $repo"
  ) &
done
wait
