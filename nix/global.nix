let
    pkgs = import ./nixpkgs.nix {};
    work = (import ../../code/simspace/ps/dev {}).pkgs;

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
    # minimal derivation, ensures that we depend on all bootstrap envs
    bootstrap-prebuild = with pkgs; (derivation {
      name = "bootstrap-envs";
      builder = "${bash}/bin/bash";
      args = [ "-c" "$coreutils/bin/mkdir $out; echo foo > $out/bootstrap-envs" ];
      system = builtins.currentSystem;
      inherit coreutils;
      inherit (bootstrap) haskell javascript psql python;
    });

    bergey = {
      global = with pkgs; [
        age
        aspell
        aspellDicts.en
        atool
        bash-completion
        bench
        direnv
        dtach
        editorconfig-core-c
        git
        git-lfs
        gitAndTools.hub
        gnumake
        gnupg
        gphoto2
        graphviz
        (haskell.lib.dontCheck haskellPackages.hasktags)
        haskellPackages.pandoc
        haskellPackages.hlint
        haskellPackages.graphmod
        # haskellPackages.HaRe
        (haskellPackages.callPackage sizes {})
        haskellPackages.wai-app-static # warp
        htop
        id3v2
        imagemagick
        jq yq
        keybase
        kubectl
        lftp
        loc
        mr
        msmtp
        niv
        nix-prefetch-git
        nmap
        nodePackages.jsonlint
        pass
        perlPackages.ImageExifTool
        ripgrep
        rsync
        rustup
        shellcheck
        stack
        stow
        stylish-haskell
        textql
        tmux
        w3m
        watch
        wget
        wireshark
        xlsfonts
        yaml2json
        python3.pkgs.yamllint
      ];

      linux = with pkgs; [
        # (agda.withPackages (a: [ a.standard-library ]))
        acpi
        borgbackup
        curl
        docker
        file
        gitAndTools.git-annex
        inotifyTools
        linuxPackages.virtualbox
        lrzip # broken 2021-03-03 MacOS
        psmisc # pstree &c
      ];

      darwin = with pkgs; [
        bashInteractive
        govc
        nix
        powershell
      ];

      simspace = with work; [
        minio
        nodejs
        haskellPackages.ormolu
        postgresqlWithPackages
        rabbitmq-server
        stern
        yarn
        xsv
        zstd
      ];

      workstation = with pkgs; [
        bootstrap-prebuild
        google-cloud-sdk
        inkscape
      ];

      linux-workstation = with pkgs; [
        alacritty
        arduino
        # calibre # broken apsw 2020-06-21
        crawl
        dmenu
        dropbox-cli
        feh
        # freeciv_gtk
        maim # screenshots
        pavucontrol
        slack
        transmission
        xorg.xev
        zathura
        # zoom-us
      ];

      linux-server = [];
    };

in rec {
  linux-workstation = pkgs.buildEnv {
    name = "bergey-linux-workstation";
    paths = with bergey; global ++ linux ++ workstation ++ bergey.linux-workstation;
  };

  linux-server = pkgs.buildEnv {
    name = "bergey-linux-server";
    paths = with bergey; global ++ linux ++ linux-server;
  };

  darwin = pkgs.buildEnv {
    name = "bergey-darwin";
    # it happens that my Mac is for work & my Linux boxen aren't
    paths = with bergey; global ++ bergey.darwin ++ workstation ++ simspace;
  };

  default = if pkgs.stdenv.isDarwin then darwin else linux-workstation;
}
