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
      inherit (bootstrap) ruby javascript;
    });

    # function to help hold back a single package from system upgrades
    pinned = args: let
      pkgs = import ./nixpkgs.nix {
        snapshot = {inherit (args) rev sha256;};
      };
    in args.package pkgs;

    kits = {
      global = (with pkgs; [
        aspell
        aspellDicts.en
        atool
        bash-completion
        brotli
        direnv
        dtach
        editorconfig-core-c
        fastmod
        gh
        git
        git-lfs
        gnumake
        gnupg
        gopls
        graphviz
        htop
        jq yq
        jujutsu
        just
        kondo
        kubectl
        lftp
        lrzip
        mise
        mr
        nix-prefetch-git
        nix-tree
        nmap
        nodePackages.jsonlint
        nodePackages.typescript-language-server
        ripgrep
        rsync
        rustup
        # rust-analyzer-unwrapped
        shellcheck
        stow
        textql
        tmux
        typos
        watch
        wget
        (pinned {
          package = (pkgs: pkgs.wireshark);
          rev = "18dd725c29603f582cf1900e0d25f9f1063dbf11";
          sha256 = "0zrp7w41vqln7mmhvpb8ww6g6807bhic5c72mkqf9qh5336vc13b";
        })
        wrk
        xlsfonts
        xz
        yaml2json
        python3.pkgs.yamllint
        zstd

        # rust posix replacements / enhancements
        bat
        choose
        du-dust # https://github.com/bootandy/dust
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
        inotifyTools
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
        bootstrap-prebuild
        capnproto 
        capnproto-rust
        cargo-dist
        oranda
        ledger
        google-cloud-sdk
        (pinned {
          package = pp: pp.rWrapper.override{ packages = with pp.rPackages; [ tidyverse promr ]; };
          rev = "85f7e662eda4fa3a995556527c87b2524b691933";
          sha256 = "1p8qam6pixcin63wai3y55bcyfi1i8525s1hh17177cqchh1j117";
        })
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
        maim # screenshots
        pavucontrol
        perlPackages.ImageExifTool
        slack
        transmission_4
        xorg.xev
        zathura
        inkscape # broken M1 2022-06-16
        zotero # broken M1 2022-05-03
      ];

      # some things don't work on ubuntu?
      nixos = with pkgs; [
        calibre
        dropbox-cli
      ];

      server = [];

      braze = with pkgs; [
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

  BZUSWVX02L7L7Q = pkgs.buildEnv { # Braze Macbook
    name = "bergey-braze";
    paths = with kits; global ++ darwin ++ workstation ++ braze;
  };
  BZUSCDXGX362CR = BZUSWVX02L7L7Q;
  BZCDXGX362CR = BZUSWVX02L7L7Q ;

  prandtl = pkgs.buildEnv {
    name = "bergey-linux-workstation";
    paths = with kits; global ++ linux ++ workstation ++ linux-workstation ++ nixos;
  };
}
