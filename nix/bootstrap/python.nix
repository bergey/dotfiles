let
    pkgs = import ../nixpkgs.nix {};
  
    mkEnv = env: if pkgs.lib.inNixShell
          then pkgs.mkShell (env // {buildInputs = env.paths;})
          else pkgs.buildEnv env;

    python = pkgs.python37;

in with python.pkgs; mkEnv {
        name = "bootstrap-python";
        paths = [
            flake8
            ipython
            python
            pip
            virtualenv
        ];
    }
