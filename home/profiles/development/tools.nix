{ config
, lib
, pkgs
, ...
}: {
  # FIXME: most of these should NOT be considered part of "core"
  # most don't belong on a server, etc.
  home.packages = with pkgs; [
    ## === Uncategorized Packages ===
    hyperfine   # <- Command-line benchmarking tool
    chronologer # <- Visualize changes in benchmark timings over git history

    ## === Local Development ===
    reuse #           <- Tool for licensing compliance
    asciinema #       <- Record and share terminal sessions
    universal-ctags # <- Generates tag files in case of LSP bankrupcy
    just #            <- Just a command runner
    tokei #           <- Fancy wordcount for programmers
    sd #              <- Modern sed replacement
    grex #            <- Generate regexps from user-provided test cases.
    httpie #          <- Modern, user-friendly command-line HTTP client for the API era.
    pastel #          <- A command-line tool to generate, analyze, convert and manipulate colors
    hexyl #           <- a command-line hex viewer
    glow #            <- a markdown cli renderer (by charmbracelet)

    ## === Nix Utilities ===
    nickel #        <-
    fup-repl #      <- flake-utils-plus repl
    nvfetcher-bin # <- Generate nix sources expression for the latest version of packages
    nix-diff #      <- Explain why two Nix derivations differ
    nix-tree #      <- Interactively browse dependency graphs of Nix derivations.
    nix-top #       <- 'top' for nix builds
    manix #         <- nix documentation search
    nix-du #        <- Analyze derivation and store size
    zgrviewer #     <- Graphviz/DOT viewer (often used with nix-du)

    ## === Linters + Formatters ===
    shfmt
    shellcheck
    yamllint
    statix
    deadnix
    nixpkg-fmt
    nixfmt
    alejandra
    cachix
    treefmt
  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    sourcetrail # <- Rest in peace sourcetrail, the best C++ exploration tool ever to live.
  ]);

  fonts.fontconfig.enable = true;

  # TODO: check these out
  # services.etebase = {};
  # services.etebase.sync = {};
  # services.flameshot = {};
  # services.hound = ...
  # programs.ncspot = {}; # spotify thingy
}
