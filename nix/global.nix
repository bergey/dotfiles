let
    pkgs = import ./nixpkgs.nix {};

    sizes =
        ({ mkDerivation, base, bytestring, cmdargs, deepseq, dlist, lens
        , parallel-io, regex-posix, system-fileio, system-filepath, text
        , unix
        }:
        mkDerivation {
            pname = "sizes";
            version = "2.3.2-git";
            src = pkgs.fetchgit {
                url = "https://github.com/jwiegley/sizes.git";
                sha256 = "0ma8kxw1sh3bg2rqgq7i6pbjg5frjvyyjshma8q98r8sqa579yn2";
                rev = "1658de1c70d505f2e5d9d736975a8bfae77799f1";
            };
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = [
                base bytestring cmdargs deepseq dlist lens parallel-io regex-posix
                system-fileio system-filepath text unix
            ];
            description = "Recursively show space (size and i-nodes) used in subdirectories";
            license = pkgs.lib.licenses.bsd3;
            hydraPlatforms = pkgs.lib.platforms.none;
            broken = false;
            });

    bootstrap = import ./bootstrap.nix {inherit pkgs;};
    # minimal derivation, ensures that we depend on specific bootstrap envs
    bootstrap-prebuild = with pkgs; (derivation {
      name = "bootstrap-envs";
      builder = "${bash}/bin/bash";
      args = [ "-c" "$coreutils/bin/mkdir $out; echo foo > $out/bootstrap-envs" ];
      system = builtins.currentSystem;
      inherit coreutils;
      inherit (bootstrap) ruby javascript;
    });

    kits = {
      global = (with pkgs; [
        aspell
        aspellDicts.en
        atool
        bash-completion
        bench
        brotli
        direnv
        dtach
        editorconfig-core-c
        gh
        git
        git-lfs
        gitAndTools.hub
        gnumake
        gnupg
        graphviz
        (haskell.lib.dontCheck haskellPackages.hasktags)
        haskellPackages.pandoc
        haskellPackages.hlint
        (haskellPackages.callPackage sizes {})
        htop
        jq yq
        just
        keybase
        kubectl
        lftp
        loc
        lrzip
        mr
        nix-prefetch-git
        nmap
        nodePackages.jsonlint
        ripgrep
        rsync
        rustup
        rust-analyzer
        shellcheck
        stow
        textql
        tmux
        w3m
        watch
        wget
        # wireshark # broken M1 2022-05-03
        xlsfonts
        xsv
        xz
        yaml2json
        python3.pkgs.yamllint

        # rust posix replacements / enhancements
        amber
        bat
        choose
        du-dust # https://github.com/bootandy/dust
        fd
        hyperfine
        procs
        sd
        tokei
      ]);

      linux = with pkgs; [
        # (agda.withPackages (a: [ a.standard-library ]))
        acpi
        borgbackup
        curl
        docker
        file
        inotifyTools
        psmisc # pstree &c
        bpftrace
        sysstat
        perf-tools
      ];

      darwin = with pkgs; [
        bashInteractive
        nix
      ];
      # programs I install outside Nix:
      # 1Password amphetamine daisydisk karabiner magnet slack spotify xquartz zotero

      workstation = with pkgs; [
        bootstrap-prebuild
        google-cloud-sdk
      ];

      linux-workstation = with pkgs; [
        alacritty
        arduino
        calibre
        crawl
        dmenu
        dropbox-cli
        feh
        filelight
        gphoto2
        id3v2
        imagemagick
        maim # screenshots
        pavucontrol
        perlPackages.ImageExifTool
        slack
        transmission
        xorg.xev
        zathura
        inkscape # broken M1 2022-06-16
        zotero # broken M1 2022-05-03
      ];

      server = [];

      austenite = with pkgs; [
        transmission
      ];

      braze = with pkgs; [
        awscli2
        postgresql_14
        imagemagick
        snappy
        terraform
        kops
        cmake
        sops
      ];
    };

in rec {
  linux-server = pkgs.buildEnv {
    name = "bergey-linux-server";
    paths = with kits; global ++ linux ++ server;
  };

  Austenite = pkgs.buildEnv {
    name = "bergey-austenite";
    paths = (with kits; global ++ linux ++ server == austenite);
  };

  BZUSWVX02L7L7Q = pkgs.buildEnv { # Braze Macbook
    name = "bergey-braze";
    paths = with kits; global ++ darwin ++ workstation ++ braze;
  };

  prandtl = pkgs.buildEnv {
    name = "bergey-linux-workstation";
    paths = with kits; global ++ linux ++ workstation ++ linux-workstation;
  };
}
