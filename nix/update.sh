#! /usr/bin/env bash

# alternate shebang to bootstrap if the system doesn't have these tools already
#! /usr/bin/env nix-shell
#! nix-shell -i bash
#! nix-shell -p curl git jq nix

# A more general take of this idea is at
# https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/

set -euxo pipefail

REV=$(curl -L https://nixos.org/channels/nixpkgs-unstable/git-revision)
# TODO path relative to where this script resides
if [ $(jq -r .rev nix/nixpkgs-snapshot.json) = "$REV" ]
  then echo 'already on latest revision'
  else 
    [ ! -d ~/code ] && mkdir ~/code
    [ ! -d ~/code/nixpkgs ] && git clone git@github.com:NixOS/nixpkgs.git
    cd ~/code/nixpkgs
    git fetch -a --quiet
    git checkout ${REV}
    SHA=$(nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/${REV}.tar.gz)
    cd -
    jq '{owner: "NixOS", repo: "nixpkgs", rev: $rev, sha256: $sha}' <<< '{}' \
      --arg rev $REV --arg sha $SHA \
        > nixpkgs-snapshot.json
    git reset # make sure we aren't commiting anything else
    git add nixpkgs-snapshot.json
    if ! git diff --cached --exit-code  --quiet; \
        then git commit -m "$(basename $(pwd)): update nixpkgs snapshot"; \
        else echo "nothing to commit"; \
        fi
fi
