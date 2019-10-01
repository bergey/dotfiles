let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };
    pkgs = import nixpkgs { config = {}; };

myEmacs = if pkgs.stdenv.isDarwin then pkgs.emacsMacport else pkgs.emacs;
emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

in emacsWithPackages (epkgs: (with epkgs; [

    # merlin
    # modelica-mode
    # powershell
    # tuareg
    pkgs.emacsPackages.proofgeneral_HEAD
    auto-complete
    avy
    bazel-mode
    bbdb
    buffer-move
    caps-lock
    cider
    clojure-mode
    coffee-mode
    color-identifiers-mode
    company
    company-c-headers
    csv-mode
    dash
    diminish
    direnv
    dockerfile-mode
    edit-indirect
    editorconfig
    emmet-mode
    emms
    ensime
    # erlang # broken 2019-09-30
    ess
    evil
    exec-path-from-shell
    eyebrowse
    fic-mode
    flycheck
    flycheck-clojure
    flycheck-rust
    fsharp-mode
    fstar-mode
    ghc
    git-annex
    google-this
    groovy-mode
    haskell-mode
    haskell-snippets
    helm
    helm-dash
    helm-gtags
    helm-idris
    highlight-escape-sequences
    highlight-indent-guides
    highlight-quoted
    idris-mode
    intero
    ivy
    kotlin-mode
    ledger-mode
    magit
    # magit-annex # broken 2018-08-05, build needs git
    markdown-mode
    move-text
    nix-mode
    nodejs-repl
    notmuch
    org-plus-contrib
    org-pomodoro
    org-trello
    orgit
    origami
    pandoc-mode
    password-store
    perspective
    pocket-mode
    polymode
    pov-mode
    projectile
    purescript-mode
    pyvenv
    py-isort
    qml-mode
    racket-mode
    rainbow-delimiters
    rainbow-mode
    real-auto-save
    restclient
    rg
    rust-mode
    s
    sbt-mode
    scala-mode
    smartparens
    swift-mode
    systemd
    thrift
    tide
    toml-mode
    typescript-mode
    unfill
    use-package
    vala-mode
    w3m
    web-mode
    window-number
    window-purpose
    windresize
    yaml-mode
    yasnippet
    
  ]
  ))
