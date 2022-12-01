{ config
, lib
, pkgs
, ...
}: {
  home.packages = with pkgs; [
    ## === Uncategorized Packages ===
    hyperfine # <- Command-line benchmarking tool
    # chronologer # <- Visualize changes in benchmark timings over git history

    ## === Local Development ===
    reuse #           <- Tool for licensing compliance
    asciinema #       <- Record and share terminal sessions
    just #            <- Just a command runner
    tokei #           <- Fancy wordcount for programmers
    sd #              <- Modern sed replacement
    grex #            <- Generate regexps from user-provided test cases.
    httpie #          <- Modern, user-friendly command-line HTTP client for the API era.
    pastel #          <- A command-line tool to generate, analyze, convert and manipulate colors
    hexyl #           <- a command-line hex viewer
    glow #            <- a markdown cli renderer (by charmbracelet)

    ## === Nix Utilities ===
    nickel #        <- "Better configuration for less"
    nvfetcher-bin # <- Generate nix sources expression for the latest version of packages
    nix-diff #      <- Explain why two Nix derivations differ
    nix-tree #      <- Interactively browse dependency graphs of Nix derivations.
    nix-top #       <- 'top' for nix builds
    nixos-option #  <- Search nixos options
    manix #         <- nix documentation search
    nix-du #        <- Analyze derivation and store size
    nox #           <- Tools to make nix nicer to use
    zgrviewer #     <- Graphviz/DOT viewer (often used with nix-du)
    nix-update #    <- swiss-army knife for updating nix packages
    nix-bisect #    <- Helper for bisecting nix builds
    devenv #        <- Cachix's new 'devenv' tool
    vulnix #        <- Scan nix (store) paths for CVEs
    nix-eval-jobs # <- Parallel nix evaluator with a streamable json output

    ## === Linters + Formatters ===
    shfmt
    shellcheck
    yamllint
    statix
    deadnix
    nixpkgs-fmt
    nixfmt
    alejandra
    cachix
    treefmt
  ];

  fonts.fontconfig.enable = true;
}
