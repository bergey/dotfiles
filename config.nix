pkgs :
{
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = self: with self; rec {

    haskellTools = spec: ([
        spec.ghc
        sloccount
    ] ++ (with spec.hsPkgs; [
        hlint
        cabalInstall
    ]) ++ (with haskellPackages_ghc784; [
        cabal2nix
        hasktags
        # threadscope # broken 2014-11-19
    ]));

    ghcEnv = spec: myEnvFun {
        name = spec.name;
        buildInputs = haskellTools spec ++ myHaskellPackages spec;
    };

    ghcEnv_742 = ghcEnv {
        name = "ghc742";
        ghc = ghc.ghc742;
        hsPkgs = haskellPackages_ghc742;
    };

    ghcEnv_763 = ghcEnv {
        name = "ghc763";
        ghc = ghc.ghc763;
        hsPkgs = haskellPackages_ghc763;
    };

    ghcSpec_784 = {
        name = "ghc784";
        ghc = ghc.ghc784;
        hsPkgs = haskellPackages_ghc784;
    };

    ghcEnv_784 = ghcEnv ghcSpec_784;

    ghcEnvBare = myEnvFun {
      name = "ghc78-bare";
      buildInputs = [ ghc.ghc784 haskellPackages_ghc784.cabalInstall ];
    };

    vcsTools = self.buildEnv {
        name = "vcsTools";
        paths = [
            subversion
            git
            bazaar
            darcs
            mercurial
            gitAndTools.hub
            mr
        ];
    };

    graphicsTools = buildEnv {
	name = "graphicsTools";
	paths = [
            inkscape
            gimp_2_8
            vlc
        ];
    };

    officeTools = buildEnv {
        name = "officeTools";
        paths = [
            calibre
            libreoffice
        ];
    };

    rToolsEnv = buildEnv {
      name = "rTools";
      paths = with rPackages; [
        devtools
        ggplot2
        R
      ];
    };

    photoTools = buildEnv {
      name = "photoTools";
      paths = [
        gphoto2
        darktable
        gimp
        imagemagick
        perlPackages.ImageExifTool
      ];
    };

    cadTools = buildEnv {
      name = "cadTools";
      paths = [
        meshlab
        slic3r
        openscad
      ];
    };

    webBrowsers = buildEnv {
      name = "webBrowsers";
      paths = [
        firefox-bin
        chromium
        elinks
        # for comparison
        # uzbl
        # dwb
        # netsurf
      ];
};

      docTools = buildEnv {
        name = "docTools";
        paths = [
          calibre
          # libreoffice
          haskellPackages.pandoc
          haskellPackages.pandocCiteproc
          # texLiveFull
          zathura
        ];
      };

      Pandoc = buildEnv {
        name = "Pandoc";
        paths = [
          haskellPackages.pandoc
          haskellPackages.pandocCiteproc
        ];
      };

      avTools = buildEnv {
        name = "avTools";
        paths = [
          abcde
          id3v2
          vlc
        ];
      };

      vmTools = buildEnv {
        name = "vmTools";
        paths = [
          vagrant
          linuxPackages.virtualbox
        ];
      };

      docutilsEnv = myEnvFun {
          name = "docutils";
          # ps1 = "[\\$NIX_MYENV_NAME] \\t \\# \\h \\\\\\$? $ ";
          buildInputs = with python33Packages; [
              haskellPackages.pandoc
              ipython
              docutils
              pygments
          ];
      };
      pythonEnv = myEnvFun {
        name = "python";
        # ps1 = "[\\$NIX_MYENV_NAME] \\t \\# \\h \\\\\\$? $ ";
        buildInputs = with python33Packages; [
            git
            stdenv
            zlib
            ipython
            pandas
            matplotlib
            scipy
            # tkinter
            notmuch
            self.notmuch
            ];
      };

      # jsEnv = myEnvFun {
      jsTools = buildEnv {
        name = "jsTools";
        # buildInputs = [
        paths = [
              # nodePackages.browserify
              nodePackages.jshint
              nodePackages.gulp
              nodePackages.grunt-cli
              nodePackages.mocha
              nodejs
              rubyLibs.sass
        ];
    };

    # rarely used
    androidEnv = myEnvFun {
      name = "android";
      buildInputs = [
          davfs
          wdfs-fuse
          androidsdk
      ];
    };


    arduinoEnv = myEnvFun {
        name = "arduino";
        buildInputs = [
            arduino_core
            avrgcclibc
            avrdude
            ino
            stdenv
        ];
    };

    scalaEnv = myEnvFun {
        name = "scala";
        buildInputs = [
            sbt
            scala
        ];
    };

    clojureEnv = myEnvFun {
        name = "clojure";
        buildInputs = [
            leiningen
            clojure
        ];
    };

    rustEnv = myEnvFun {
        name = "rust";
        buildInputs = [
            rust
        ];
    };

    # this is very long!
    myHaskellPackages = spec: with spec.hsPkgs; [
        HUnit
        aeson
        attoparsec
        cassava
        cereal
        colour
        conduit
        csv-conduit
        dataDefault
        diagramsCairo
        diagramsContrib
        diagramsLib
        digest
        glib
        gtk
        # hakyll # broken 2014-11-19
        haskellSrcExts
        highlightingKate
        hinotify
        hint
        hsBibutils
        hslogger
        hspec
        # ncurses # broken 2014-11-19
        network
        optparseApplicative
        pandoc
        pango
        parsec
        primitive
        random
        regexPosix
        semigroups
        shelly
        tasty
        terminfo
        testFramework
        text
        textIcu
        transformers
        unixCompat
        unorderedContainers
        utf8String
        zlib
        cairo
    ]

    ++ stdenv.lib.optionals
        (stdenv.lib.versionOlder "7.7" spec.ghc.version)
        # Packages that only work in 7.8+
        [   units
            criterion
        ]

    ++ stdenv.lib.optionals
        (stdenv.lib.versionOlder "7.5" spec.ghc.version)
        # Packages that only work in 7.6+
        [
            linear
            lens
        ];

    myHoogleLocal = spec: spec.hsPkgs.hoogleLocal.override {
        packages = myHaskellPackages spec;
    };

    hoogleLocal_784 = myHoogleLocal ghcSpec_784;

    };
}
