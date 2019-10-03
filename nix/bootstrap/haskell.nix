let
    nixpkgs = let
        # let snapshot = builtins.fromJSON (builtins.readFile ../nixpkgs-snapshot.json);
        # inherit (snapshot) owner repo rev;
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "d484f2b7fc0834a068e8ace851faa449a03963f5";
      in builtins.fetchTarball {
        # inherit (snapshot) sha256;
        sha256 = "0jk93ikryi2hqc30l2n5i4vlgmklrlzb8cf7b3sg1q3k70q344jn";
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
        };
    pkgs = import nixpkgs {};

    mkEnv = env: if pkgs.lib.inNixShell
        then pkgs.mkShell (env // {buildInputs = env.paths;})
        else pkgs.buildEnv env;

in with pkgs; mkEnv {
        name = "bootstrap-haskell";
        paths = [
            cabal2nix
            cabal-install
            # ghc
            haskellPackages.hpack
            haskellPackages.shake
            # haskellPackages.alex
            stack
            (haskell.packages.ghc865.ghcWithPackages
                (ps: [ ps.shake ]))
        ];
    }
