{
  config,
  lib,
  pkgs,
  ...
}: {
  # FIXME: most of these should NOT be considered part of "core"
  # most don't belong on a server, etc.
  home.packages = with pkgs; [
    ## === Uncategorized Packages ===
    ncdu
    iperf3

    ## === Local Development ===

    act # Run GitHub Actions locally
    asciinema
    circleci-cli
    hyperfine
    universal-ctags

    ## === Nix Development ===
    fup-repl

    nixfmt

    nix-tree
    nix-du
    nix-top
    zgrviewer

    ## === Data and Documents ===

    tidy-viewer # Pretty-print CSV files
    xsv # A fast CSV command line toolkit
    # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
    python3Packages.yq

    # TODO: fails to build on darwin as of 2022-05-04
    # nodePackages.mermaid-cli # https://github.com/mermaid-js/mermaid-cli

    ## === Linters + Formatters ===

    shfmt
    shellcheck
    yamllint
    statix

    ## === Media Tools ===

    # TODO: use hm module
    mpv
    youtube-dl

    ## === Data Sync ===

    rclone
  ];

  # TODO: check these out
  # services.etebase = {};
  # services.etebase.sync = {};
  # services.flameshot = {};
  # services.hound = ...
  # programs.ncspot = {}; # spotify thingy
}
