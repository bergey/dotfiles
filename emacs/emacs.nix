let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ../nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };
    pkgs = import ../nix/nixpkgs.nix {};

in (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: (with epkgs; [
    # agda2-mode # broken on M1? 2022-05-07
    # pkgs.emacsPackages.proofgeneral_HEAD
    add-node-modules-path
    buffer-move
    capnp-mode
    clojure-mode
    coffee-mode
    color-identifiers-mode
    company
    company-c-headers
    counsel
    csv-mode
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
    flycheck-haskell
    fstar-mode
    git-link
    go-mode
    groovy-mode
    haskell-mode
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
    native-complete
    nginx-mode
    nix-mode
    nix-sandbox
    nodejs-repl
    org-cliplink
    orgit
    origami
    ox-jira
    pandoc-mode
    persp-projectile
    perspective
    poly-markdown
    polymode
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
    rubocop
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
    treesit-grammars.with-all-grammars
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
