let
    pkgs = import ../nixpkgs.nix {};
  
    mkEnv = env: if pkgs.lib.inNixShell
          then pkgs.mkShell (env // {buildInputs = env.paths;})
          else pkgs.buildEnv env;

in mkEnv {
        name = "bootstrap-java";
        paths = [
          pkgs.jdk11
        ];
    }
