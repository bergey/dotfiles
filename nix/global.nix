let
    pkgs = import ./nixpkgs.nix {};

    bootstrap = import ./bootstrap.nix {inherit pkgs;};
    # minimal derivation, ensures that we depend on specific bootstrap envs
    bootstrap-prebuild = with pkgs; (derivation {
      name = "bootstrap-envs";
      builder = "${bash}/bin/bash";
      args = [ "-c" "$coreutils/bin/mkdir $out; echo foo > $out/bootstrap-envs" ];
      system = builtins.currentSystem;
      inherit coreutils;
      inherit (bootstrap) javascript;
    });

    # function to help hold back a single package from system upgrades
    pinned = args: let
      pkgs = import ./nixpkgs.nix {
        snapshot = {inherit (args) rev sha256;};
      };
    in args.package pkgs;

    kits = {
      global = (with pkgs; [
        atool
        bash-completion
        brotli
        dtach
        git
        gnumake
        htop
        jq yq
        just
        lftp
        lrzip
        nmap
        rsync
        shellcheck
        watch
        wget
        xz
        zstd

        # rust posix replacements / enhancements
        bat
        choose
        dust # https://github.com/bootandy/dust
        fd
        hexyl
        hyperfine
        procs
        sd
        tokei
      ]);

      linux = with pkgs; [
        # (agda.withPackages (a: [ a.standard-library ]))
        acpi
        borgbackup
        clickhouse
        curl
        docker
        file
        gcc
        inotify-tools
        lsof
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
        aspell
        aspellDicts.en
        bootstrap-prebuild
        capnproto 
        capnproto-rust
        cargo-dist
        (pinned { # broken on macos
          package = (pkgs: pkgs.direnv);
          rev = "f8573b9c935cfaa162dd62cc9e75ae2db86f85df";
          sha256 = "01j24h5r9cypqsq9nkznakckp3r9z9fpp8vngks1pxya7p9wg5c6";
        })
        editorconfig-core-c
        fastmod
        gh
        git-lfs
        gnupg
        google-cloud-sdk
        gopls
        graphviz
        kondo
        kubectl
        ledger
        (pinned { # broken on macos, cgo
          package = (pkgs: pkgs.mise);
          rev = "f8573b9c935cfaa162dd62cc9e75ae2db86f85df";
          sha256 = "01j24h5r9cypqsq9nkznakckp3r9z9fpp8vngks1pxya7p9wg5c6";
        })
        mr
        nix-prefetch-git
        nix-tree
        oranda
        ripgrep
        rustup
        sqlite
        stow
        textql
        tmux
        typescript-language-server
        typos
        (pinned { # R
          package = pp: pp.rWrapper.override{ packages = with pp.rPackages; [ tidyverse promr ]; };
          rev = "85f7e662eda4fa3a995556527c87b2524b691933";
          sha256 = "1p8qam6pixcin63wai3y55bcyfi1i8525s1hh17177cqchh1j117";
        })
        (pinned {
          package = (pkgs: pkgs.wireshark);
          rev = "18dd725c29603f582cf1900e0d25f9f1063dbf11";
          sha256 = "0zrp7w41vqln7mmhvpb8ww6g6807bhic5c72mkqf9qh5336vc13b";
        })
        wrk
        xlsfonts
        yaml2json
        python3.pkgs.yamllint
        zed-editor
      ];

      linux-workstation = with pkgs; [
        alacritty
        arduino
        crawl
        feh
        kdePackages.filelight
        gphoto2
        id3v2
        imagemagick
        jujutsu
        maim # screenshots
        inkscape # broken M1 2022-06-16
        pavucontrol
        perlPackages.ImageExifTool
        transmission_4
        xev
        yarn
        zathura
        zotero # broken M1 2022-05-03
      ];

      # some things don't work on ubuntu?
      nixos = with pkgs; [
        calibre
        dropbox-cli
      ];

      server = [];

      braze_ = with pkgs; [
        awscli2
        cmake
        imagemagick
        kops
        postman
        snappy
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
    paths = with kits; global ++ linux ++ workstation ++ linux-workstation;
  };

  braze = pkgs.buildEnv { # Braze Macbook
    name = "bergey-braze";
    paths = with kits; global ++ darwin ++ workstation ++ braze_;
  };
  BZUSWVX02L7L7Q = braze;
  BZUSCDXGX362CR = braze;
  BZCDXGX362CR = braze ;

  prandtl = pkgs.buildEnv {
    name = "bergey-linux-workstation";
    paths = with kits; global ++ linux ++ workstation ++ linux-workstation ++ nixos;
  };
}
