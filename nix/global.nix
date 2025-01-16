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
        clickhouse
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
        loc
        lrzip
        mr
        nix-prefetch-git
        nix-tree
        nmap
        nodePackages.jsonlint
        nodePackages.degit
        nodePackages.eslint
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
        wireshark
        wrk
        xlsfonts
        xsv
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
        transmission_4
        xorg.xev
        zathura
        inkscape # broken M1 2022-06-16
        zotero # broken M1 2022-05-03
      ];

      server = [];

      austenite = with pkgs; [
        transmission_4
      ];

      braze = with pkgs; [
        awscli2
        postgresql_14
        imagemagick
        snappy
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
    paths = with kits; global ++ linux ++ server ++ austenite;
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
