{
  description = "my emacs config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

  };

  outputs = {self, nixpkgs}:
    {
      packages = builtins.mapAttrs (system: pkgs:
        {
          default = import ./emacs.nix { inherit pkgs; };
        }) nixpkgs.legacyPackages;
    };
}
