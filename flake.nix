{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

  };

  outputs = {nixpkgs}:
    let
      pkgs = import nixpkgs {
        config = {
            allowUnfree = true;
        };
      };
      kits = import ./global.nix { inherit pkgs; };
    in {
      # TODO these only make sense for Linux, and mostly x86
      # do I really want mapAttrs this way?
      packages = builtins.mapAttrs (system: pkgs: {
        linux-server = pkgs.buildEnv {
          name = "bergey-linux-server";
          paths = with kits; global ++ linux ++ server;
        };

        austenite = pkgs.buildEnv {
          name = "bergey-austenite";
          paths = with kits; global ++ linux ++ workstation ++ linux-workstation;
        };

        prandtl = pkgs.buildEnv {
          name = "bergey-linux-workstation";
          paths = with kits; global ++ linux ++ workstation ++ linux-workstation ++ nixos;
        };
      }) nixpkgs.legacyPackages;
    };
}
