let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ../nixpkgs-snapshot.json);
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

in (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: (with epkgs; [

    # agda2-mode # broken on M1? 2022-05-07
    # pkgs.emacsPackages.proofgeneral_HEAD
    add-node-modules-path
    buffer-move
    clojure-mode
    coffee-mode
    color-identifiers-mode
    company
    company-c-headers
    counsel
    csv-mode
    deadgrep
    default-text-scale
    diminish
    direnv
    dockerfile-mode
    edit-indirect
    editorconfig
    emmet-mode
    erlang
    ess
    evil
    evil-collection
    exec-path-from-shell
    feature-mode
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
    highlight-escape-sequences
    highlight-indent-guides
    highlight-quoted
    idris-mode
    inf-ruby
    ivy
    just-mode
    kotlin-mode
    ledger-mode
    lsp-haskell
    lsp-ivy
    lsp-mode
    magit
    markdown-mode
    move-text
    native-complete
    nginx-mode
    nix-mode
    nix-sandbox
    nodejs-repl
    nov
    org-cliplink
    orgit
    origami
    ox-jira
    pandoc-mode
    password-store
    persp-projectile
    perspective
    poly-markdown
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
    ruby-test-mode
    rust-mode
    s
    sbt-mode
    scala-mode
    sed-mode
    smartparens
    swift-mode
    systemd
    terraform-mode
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
