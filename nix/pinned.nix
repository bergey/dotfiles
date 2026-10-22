# function to help hold back a single package from system upgrades
{rev, sha256, package, system}:
let
  nixpkgs = builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  };
  pkgs = import nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
    };
  };
in package pkgs
