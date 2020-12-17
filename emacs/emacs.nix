let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };
    pkgs = import nixpkgs { 
       overlays = [
         (import (builtins.fetchTarball {
           url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
         }))
       ];
    };

in {
  pkgs = pkgs;
  emacs = (pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      # Package is optional, defaults to pkgs.emacs
      # package = pkgs.emacsGit;
      alwaysEnsure = true;
  });
}
