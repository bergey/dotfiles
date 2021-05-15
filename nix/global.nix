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

    bootstrap = import ./bootstrap.nix;

in with pkgs;

buildEnv {
  name = "bergey-env";
  paths= [
    alacritty
    ansible
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
    inkscape
    jq yq
    keybase
    kubectl
    kubernetes-helm 
    ledger
    lftp
    loc
    # lrzip # broken 2021-03-03 MacOS
    mr
    msmtp
    niv
    nix-prefetch-git
    nmap
    nodePackages.jsonlint
    notmuch
    pass
    perlPackages.ImageExifTool
    pwgen
    ripgrep
    rsync
   stack
    stow
    stylish-haskell
    textql
    tmux
    # vagrant
    w3m
    wget
    wireshark
    xlsfonts
    yaml2json
    # python3.pkgs.yamllint # broken 2021-03-22

    # minimal derivation, ensures that we depend on all bootstrap envs
    (derivation (bootstrap // {
      name = "bootstrap-envs";
      builder = "${bash}/bin/bash";
      args = [ "-c" "$coreutils/bin/mkdir $out; echo foo > $out/bootstrap-envs" ];
      system = builtins.currentSystem;
      inherit coreutils;
    }))

    ] ++ (if stdenv.isDarwin then [
      govc
      nix
      # powershell # broken 2021-03-22
      # it happens that my Mac is for work & my Linux boxen aren't
      work.yarn
      work.nodejs
      work.postgresqlWithPackages
      work.minio
      work.zstd
    ] else [ # Linux
        # (agda.withPackages (a: [ a.standard-library ]))
        acpi
        arduino
        borgbackup
        # calibre # broken apsw 2020-06-21
        crawl
        curl
        dmenu
        docker
        dropbox-cli
        feh
        file
        # freeciv_gtk
        gitAndTools.git-annex
        google-cloud-sdk
        inotifyTools
        isync # mbsync
        linuxPackages.virtualbox
        maim # screenshots
        pavucontrol
        psmisc # pstree &c
        slack
        transmission
        unison
        xorg.xev
        zathura
        # zoom-us
    ]);
  }
