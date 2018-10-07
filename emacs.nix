let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev = "0a7e258012b60cbe530a756f09a4f2516786d370";
    sha256 = "1qcnxkqkw7bffyc17mqifcwjfqwbvn0vs0xgxnjvh9w0ssl2s036";
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
    dockerfile-mode
    edit-indirect
    editorconfig
    emmet-mode
    emms
    ensime
    erlang
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
    org-trello
    orgit
    pandoc-mode
    password-store
    perspective
    pocket-mode
    polymode
    pov-mode
    projectile
    purescript-mode
    pyvenv
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
