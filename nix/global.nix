{pkgs, system}:
let
    bootstrap = import ./bootstrap.nix {inherit pkgs;};
    # minimal derivation, ensures that we depend on specific bootstrap envs
    bootstrap-prebuild = with pkgs; (derivation {
      name = "bootstrap-envs";
      builder = "${bash}/bin/bash";
      args = [ "-c" "$coreutils/bin/mkdir $out; echo foo > $out/bootstrap-envs" ];
      inherit coreutils system;
      inherit (bootstrap) javascript python;
    });

    # function to help hold back a single package from system upgrades
    pinned = {rev, sha256, package}: let
      nixpkgs = builtins.fetchTarball {
        inherit sha256;
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      };
      pkgs = import nixpkgs {
        config = {
          allowUnfree = true;
        };
      };
    in package pkgs;

    in {
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
        rclone
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
        cargo-dist
        direnv
        fastmod
        gnupg
        google-cloud-sdk
        graphviz
        bootstrap.julia # global so emacs comint can find it
        kondo
        kubectl
        mise
        mr
        nix-prefetch-git
        nix-tree
        oranda
        ripgrep
        rustup
        sqlite
        stow
        textql
        typescript-language-server
        typos
        (rWrapper.override{ packages = with rPackages; [ tidyverse promr ]; })
        wireshark
        wrk
        xlsfonts
        yaml2json
        python3.pkgs.yamllint
      ];

      linux-workstation = with pkgs; [
        alacritty
        crawl
        gphoto2
        id3v2
        imagemagick
        isync
        jujutsu
        maim # screenshots
        inkscape # broken M1 2022-06-16
        pavucontrol
        perlPackages.ImageExifTool
        transmission_4
        xev
        zathura
        zotero # broken M1 2022-05-03
      ];

      # some things don't work on ubuntu?
      nixos = with pkgs; [
        calibre
        dropbox-cli
      ];

      server = [];
    }
