# -*- mode: sh; -*-
# various conveniences for working with Nix
# I call this from .bashrc

alias n='nix-env'
alias up='sudo nixos-rebuild switch --upgrade'
alias ngc='sudo nix-collect-garbage --delete-older-than 7d'

# install by local attribute
function nia {
    nix-env -iA nixos.pkgs.$1
}

# switch Nix profiles
function np { 
         nix-env -S /nix/var/nix/profiles/per-user/$USER/$1
}

# name of current Nix profile
alias whichp='basename $(readlink ~/.nix-profile)'

# helper function for cabalc below
function incl_path {
         outpath=$(nix-env -qa $1 --out-path --no-name)
         echo -n "--extra-include-dirs=$outpath/include --extra-lib-dirs=$outpath/lib "
}

# cabal install with a bunch of nix library paths added
# I don't use this anymore, in favor of nix-shell, but maybe it's useful to someone.
function cabalc {
         pkg=$1
         shift
         cabal install $pkg $(for n in $@; do incl_path $n; done)
}

# shorter nix-shell commands
alias nsh='nix-shell --pure'
alias nixc='eval "$configurePhase"'
alias nixb='eval "$buildPhase"'

# make .nix files for local haskell packages
# presumably some of this will be superceded by the most recent
# cabal2nix, & haskell-ng

# cabal2nix for local packages
function cabal2nix-local { 
    cabal2nix ./. > default.nix
    cat - > shell.nix <<EOF
{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages }:

let 
  hs = haskellPackages.override {
        extension = self: super: rec {
          hsPkg = pkg: version: self.callPackage "/home/bergey/code/nixHaskellVersioned/\${pkg}/\${version}.nix" {};
          thisPackage = self.callPackage ./. {};
      };
    };
  in
      pkgs.lib.overrideDerivation hs.thisPackage (attrs: {
       buildInputs = [hs.cabalInstall ] ++ attrs.buildInputs;
 })
EOF
}

# create a .nix file for a particular version, in a known location.
# together with the hsPkg Nix function above, allows nix-shell with
# explicit versions, which need not be in nixpkgs.
function cabal2nix-overlay {
         local pathFragment=$(echo $1 | sed 's|\(.*\)-\(.*\)|\1/\2.nix|')
         local dest=$HOME/code/nixHaskellVersioned/$pathFragment
         mkdir -p ${dest%/*}
         cabal2nix cabal://$1 > $dest
}
