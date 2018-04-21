let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
     rev = "ea145b68a019f6fff89e772e9a6c5f0584acc02c";
     sha256 = "0jn9m4lrzrzfz1a45xpxd2pvxyy8lapwjsyavaraiqaygd1lhzh6";
  };

    pkgs = import nixpkgs { config = {}; };
myEmacs = if pkgs.stdenv.isDarwin then pkgs.emacsMacport else pkgs.emacs;
emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

in emacsWithPackages (epkgs: (with epkgs; [

    # merlin
    # modelica-mode
    # powershell
    # tuareg
    anaconda-mode
    auto-complete
    avy
    bazel-mode
    bbdb
    buffer-move
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
    magit-annex
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
++ (if pkgs.stdenv.isDarwin then [] else [
    python-mode
])
  ))
