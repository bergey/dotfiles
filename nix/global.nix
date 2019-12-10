let
    pkgs = import ./nixpkgs.nix;

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
            license = pkgs.stdenv.lib.licenses.bsd3;
            hydraPlatforms = pkgs.stdenv.lib.platforms.none;
            broken = false;
            });

in with pkgs;

buildEnv {
  name = "bergey-env";
  paths= [
    alacritty
    aspell
    aspellDicts.en
    atool
    bench
#    ctags
    direnv
    editorconfig-core-c
    git
    git-lfs
    gitAndTools.hub
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
    isync # mbsync
    jq
    keybase
    ledger3
    lftp
    lrzip
    mr
    msmtp
    nix-prefetch-git
    nmap
    nodePackages.jsonlint
    notmuch
    pass
    perlPackages.ImageExifTool
    pwgen
    ripgrep
    rsync
    # stack
    stow
    textql
    tmux
    unison
    # vagrant
    w3m
    wget
    xlsfonts
    yaml2json
    python.pkgs.yamllint
    ] ++ (if stdenv.isDarwin then [
        nix
    ] else [
        acpi
        arduino
        borgbackup
        cadaver
        calibre
        crawl
        curl
        dmenu
        docker
        dropbox-cli
        feh
        file
        # freeciv_gtk
        gitAndTools.git-annex
        gnumake
        google-cloud-sdk
        inotifyTools
        kubectl
        linuxPackages.virtualbox
        loc
        maim # screenshots
        pavucontrol
        psmisc # pstree &c
        slack
        transmission
        xorg.xev
        (xscreensaver.overrideAttrs (oldAttrs: {
            patches = [ ./xscreensaver.xpm.patch ./teal.xpm.patch ];
        }))
        zathura
        zoom-us
    ]);
  }
