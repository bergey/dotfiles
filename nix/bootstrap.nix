let
  pkgs = import ./nixpkgs.nix {};

  mkBootstrap = env: if pkgs.lib.inNixShell
               then pkgs.mkShell (env // {
                 buildInputs = env.paths;
                 name = "bootstrap-" + env.name;
               })
    else pkgs.buildEnv env;

in with pkgs; {

  binary = mkBootstrap {
    name = "binary";
    paths = [
      bind
      binutils
    ] ++ (if stdenv.isDarwin then [
    ] else [
      intel-gpu-tools
      glxinfo
      opencl-info
      smem
      pciutils
    ]);
  };

  haskell = mkBootstrap {
    name = "haskell";
    paths = [
      cabal2nix
      cabal-install
      haskellPackages.alex
      haskellPackages.hpack
      haskellPackages.happy_1_20_0
      stack
      zlib
      (haskellPackages.ghcWithPackages
        (ps: with ps; [ containers vector bytestring text hashable deepseq unordered-containers shake ]))
    ];
  };

  scala = mkBootstrap {
    name = "scala";
    paths = with pkgs; [
      sbt
      scala
    ];
  };

  java = mkBootstrap {
    name = "java";
    paths = [
      pkgs.jdk11
    ];
  };

  javascript = mkBootstrap {
    name = "javascript";
    paths = with nodePackages; [
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
  };

  psql = mkBootstrap {
    name = "psql";
    paths = [
      postgresql
    ];
  };

  python = mkBootstrap {
    name = "python";
    paths = with python.pkgs; [
      flake8
      ipython
      python
      pip
      virtualenv
    ];
  };

  racket = mkBootstrap {
    name = "racket";
    paths = [
      (if stdenv.isDarwin then racket-minimal else racket)
    ];
  };

  rust = mkBootstrap {
    name = "rust";
    paths = [
      cargo
      rustc
      carnix
    ];
  };

  vcs = mkBootstrap {
    name = "vcs";
    paths = [
      cvs
      darcs
      mercurial
      subversion
    ];
  };
  
} // (if stdenv.isDarwin then {} else {

    coq = mkBootstrap {
      name = "coq";
      paths = [
        coq_8_6
        #    coqPackages_8_6.dpdgraph
        #    coqPackages_8_6.coq-ext-lib
      ];
    };

  idris = mkBootstrap {
    name = "idris";
    paths = [
      idris
    ];
  };

})
