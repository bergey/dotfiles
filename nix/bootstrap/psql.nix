let
    pkgs = import ../nixpkgs.nix {};
  
    mkEnv = env: if pkgs.lib.inNixShell
          then pkgs.mkShell (env // {buildInputs = env.paths;})
          else pkgs.buildEnv env;

in with pkgs; mkEnv {
        name = "bootstrap-psql";
    paths = [
        postgresql
        ];
    }
