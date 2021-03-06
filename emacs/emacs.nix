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
    agda2-mode
    pkgs.emacsPackages.proofgeneral_HEAD
    auto-complete
    bbdb
    buffer-move
    caps-lock
    clojure-mode
    coffee-mode
    color-identifiers-mode
    company
    company-c-headers
    counsel
    csv-mode
    dash
    default-text-scale
    diminish
    direnv
    dockerfile-mode
    edit-indirect
    editorconfig
    emmet-mode
    emms
    erlang # broken 2019-09-30
    evil
    exec-path-from-shell
    eyebrowse
    fic-mode
    flycheck
    flycheck-clojure
    flycheck-haskell
    flycheck-rust
    fstar-mode
    git-link
    google-this
    groovy-mode
    haskell-mode
    haskell-snippets
    helm
    helm-dash
    helm-gtags
    highlight-escape-sequences
    highlight-indent-guides
    highlight-quoted
    idris-mode
    ivy
    kotlin-mode
    ledger-mode
    lsp-haskell
    lsp-mode
    lsp-ui
    magit
    markdown-mode
    move-text
    native-complete
    nix-mode
    nix-sandbox
    nodejs-repl
    notmuch
    nov
    org-cliplink
    org-plus-contrib
    org-trello
    orgit
    origami
    pandoc-mode
    password-store
    perspective
    polymode
    pov-mode
    powerline
    projectile
    protobuf-mode
    purescript-mode
    py-isort
    pyvenv
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
    w3m
    web-mode
    window-number
    windresize
    yaml-mode
    yasnippet
    
  ]
  ))
