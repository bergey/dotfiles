let
  pkgs = import ./nixpkgs.nix {};

  mkBootstrap = env: if pkgs.lib.inNixShell
               then pkgs.mkShell (env // {
                 buildInputs = env.paths;
                 name = "bootstrap-" + env.name;
               })
    else pkgs.buildEnv env;

  haskellEnv = ghc: with pkgs; [
      cabal2nix
      cabal-install
      haskellPackages.alex
      haskellPackages.hpack
      haskellPackages.happy
      stack
      zlib
      ghc
    ];
  
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
    paths = haskellEnv (haskellPackages.ghcWithPackages
      (ps: with ps; [ containers vector bytestring text hashable deepseq unordered-containers shake ]));
  };

  "haskell-810" = mkBootstrap {
    name = "haskell-810";
    paths = haskellEnv haskell.compiler.ghc8102;
  };

  haskell-prof = mkBootstrap {
    name = "haskell-prof";
    paths = haskellEnv
      ((haskell.compiler.ghc883.override { ghcFlavour = "prof"; })
        .overrideAttrs (oldAttrs: rec {
          patches = [ ./0001-DYNAMIC_GHC_PROGRAMS-for-prof-build.patch ];
          # pass RTS options to ghc on every call
          # RTS opts in stack --ghc-options doesn't reach the build invocation, only configure
          postInstall = oldAttrs.postInstall + ''
            sed -i -e 's/exec "$executablename"/exec "$executablename" +RTS -p -t -s -RTS/' "$out/bin/ghc"
            '';
        }));
      # ++ (stack.overrideAttrs (oldAttrs: {
      #   patches = [ ./0001-hack-always-accept-system-ghc.patch ];
      # }))
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

  packer = mkBootstrap {
    name = "packer";
    paths = [
      packer
      r10k
      vagrant
    ];
  };

  souffle = mkBootstrap {
      name = "souffle";
      paths = [
      souffle
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
