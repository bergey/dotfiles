let
    pkgs = import ../nixpkgs.nix {};
  
    mkEnv = env: if pkgs.lib.inNixShell
          then pkgs.mkShell (env // {buildInputs = env.paths;})
          else pkgs.buildEnv env;

in with pkgs.nodePackages; mkEnv {
        name = "bootstrap-javascript";
        paths = [
            pkgs.nodejs
            node2nix
            bower
            pkgs.python3.pkgs.jsmin
            # more tools that I haven't needed in a long time
            # jshint
            # gulp
            # grunt-cli
            # mocha
            # pkgs.sass
        ];
    }
