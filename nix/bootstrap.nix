{pkgs ? import ./nixpkgs.nix {} }:

let
  mkBootstrap = env:
    if pkgs.lib.inNixShell
    then pkgs.mkShell (env // {
      buildInputs = env.paths;
      name = "bootstrap-" + env.name;
    })
    else pkgs.buildEnv {inherit (env) name paths;};

  haskellEnv = ghc: with pkgs;
    (if stdenv.isDarwin then [] else [ cabal2nix ]) ++
    [
      cabal-install
      haskellPackages.alex
      haskellPackages.hpack
      haskellPackages.happy
      stack
      zlib
      ghc
    ];
  
in with pkgs; {

  alloy = mkBootstrap {
    name = "alloy";
    paths = [
      alloy6
      minisat
      cryptominisat
    ];
  };

  ansible = mkBootstrap {
    name = "ansible";
    paths = [
      ansible
      (python3.withPackages (pyp: with pyp; [
        requests
        pyvmomi
      ]))
    ];
  };

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

  c = mkBootstrap {
    name = "c";
    paths = [
      gcc
    ];
  };

  clojure = mkBootstrap {
    name = "clojure";
    paths = [
      clojure
      leiningen
    ];
  };

  erlang = mkBootstrap {
    name = "erlang";
    paths = [
      erlang
      elixir
      rebar
    ];
  };

  haskell = mkBootstrap {
    name = "haskell";
    paths = haskellEnv (haskellPackages.ghcWithPackages
      (ps: with ps; [ containers vector bytestring text hashable deepseq unordered-containers shake ]));
  };

  "haskell-810" = mkBootstrap {
    name = "haskell-810";
    paths = haskellEnv haskell.compiler.ghc8107;
  };

  "haskell-head" = mkBootstrap {
    name = "haskell-head";
    paths = haskellEnv haskell.compiler.ghcHEAD;
  };

  # haskell-prof = mkBootstrap {
  #   name = "haskell-prof";
  #   paths = haskellEnv
  #     ((haskell.compiler.ghc884.override { ghcFlavour = "prof"; })
  #       .overrideAttrs (oldAttrs: rec {
  #         patches = [ ./0001-DYNAMIC_GHC_PROGRAMS-for-prof-build.patch ];
  #         # pass RTS options to ghc on every call
  #         # RTS opts in stack --ghc-options doesn't reach the build invocation, only configure
  #         postInstall = oldAttrs.postInstall + ''
  #           sed -i -e 's/exec "$executablename"/exec "$executablename" +RTS -p -t -s -RTS/' "$out/bin/ghc"
  #           '';
  #       }));
  #     ++ (stack.overrideAttrs (oldAttrs: {
  #       patches = [ ./0001-hack-always-accept-system-ghc.patch ];
  #     }))
  # };

  idris = mkBootstrap {
    name = "idris";
    paths = [
      idris
    ];
  };

  java = mkBootstrap {
    name = "java";
    paths = with pkgs; [
      jdk
      scala
    ];
  };

  javascript = mkBootstrap {
    name = "javascript";
    paths = with nodePackages; [
      pkgs.nodejs
      node2nix
      deno
      pkgs.python3.pkgs.jsmin
      yarn
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
    paths = with python3.pkgs; [
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

  ruby = mkBootstrap {
    name = "ruby";
    paths = with rubyPackages; [
      ruby
      solargraph
      rubocop
    ];
  };
  
  rust = mkBootstrap { # rustup simpler
    name = "rust";
    paths = [
      cargo
      rustc
      carnix
      libiconv
    ];
  };

  scala = mkBootstrap {
    name = "scala";
    paths = with pkgs; [
      sbt
      scala
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


  latex = mkBootstrap {
    name = "latex";
    paths = [
      (texlive.combine { inherit (texlive) scheme-small tex-gyre; })
    ];
    FONTCONFIG_FILE = makeFontsConf { fontDirectories = texlive.tex-gyre.pkgs; };
  };
  
  vcs = mkBootstrap {
    name = "vcs";
    paths = [
      cvs
      mercurial
      sapling
      subversion
    ];
  };

  wasm = mkBootstrap {
    name = "wasm";
    paths = [
      binaryen
      wabt
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

  souffle = mkBootstrap { # broken MacOS 2022-04-03
      name = "souffle";
      paths = [
      souffle
      ];
  };

})
