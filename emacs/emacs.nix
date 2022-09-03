let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };
    pkgs = import nixpkgs {
      config = {};
      overlays = [ (self: super: {
        ctags = self.universal-ctags;
      })];
    };

    gccEmacs = pkgs.emacs.override {
      nativeComp = true;
    };
    emacsWithPackages = (pkgs.emacsPackagesFor gccEmacs).emacsWithPackages;

in emacsWithPackages (epkgs: (with epkgs; [

    # merlin
    # modelica-mode
    # powershell
    # tuareg
    # agda2-mode # broken on M1? 2022-05-07
    # pkgs.emacsPackages.proofgeneral_HEAD
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
    erlang
    ess
    evil
    exec-path-from-shell
    eyebrowse
    fic-mode
    flycheck
    flycheck-clojure
    flycheck-haskell
    flycheck-rust
    fsharp-mode
    fstar-mode
    git-link
    go-mode
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
    just-mode
    kotlin-mode
    ledger-mode
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
    # org-plus-contrib
    org-trello
    orgit
    origami
    ox-jira
    pandoc-mode
    password-store
    perspective
    polymode
    pov-mode
    powerline
    prettier
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
    rspec-mode
    inf-ruby
    ruby-test-mode
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
