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

    pinned_wireshark = let
      # 2024-03-06 avoid error:
      # cycle detected in build of '/nix/store/xxqxfjrskz4zqlngql8hf8ffldga6mbs-jasper-4.2.1.drv' in the references of output 'lib' from output 'out'
      pkgs = import ./nixpkgs.nix {
        snapshot = {
          rev = "244ee5631a7a39b0c6bd989cdf9a1326cd3c5819";
          sha256 = "15bwld8xg790zsf1h6v8vp87rkizzr6myqv0451r75rgpvc2h2qn";
        };
      };
    in pkgs.wireshark;

    # error: builder for '/nix/store/a36sqbcyv57lgkv6kw1494n8zm95x1i0-awscli2-2.17.18.drv' failed with exit code 1;
    # > tests/unit/customizations/logs/test_startlivetail.py::LiveTailKeyBindingsTest::test_t_key_binding
    # >   /nix/store/kcm5282kss1mnil624lccr7y8wqg75vk-python3-3.11.9/lib/python3.11/enum.py:714: RuntimeWarning: coroutine 'AsyncMockMixin._execute_mock_call' was never awaited
    pinned_awscli2 = let
      pkgs = import ./nixpkgs.nix {
        snapshot = { # 2024-08-10
          rev = "154bcb95ad51bc257c2ce4043a725de6ca700ef6";
          sha256 = "0gv8wgjqldh9nr3lvpjas7sk0ffyahmvfrz5g4wd8l2r15wyk67f";
        };
      };
    in pkgs.awscli2;

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
        pinned_wireshark
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
        ledger
        google-cloud-sdk
        vscode
        (rWrapper.override{ packages = with rPackages; [ tidyverse promr ]; })
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
        pinned_awscli2
        postgresql_14
        imagemagick
        snappy
        terraform
        kops
        cmake
        sops
        vscode
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
