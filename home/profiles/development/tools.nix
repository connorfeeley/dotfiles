{ inputs', config, pkgs, ... }:
let
  toTOML = (pkgs.formats.toml { }).generate;

  inherit (pkgs.stdenv) isLinux isDarwin isAarch64;
in
{
  development.nix.enable = true;
  development.rust.enable = true;

  services.lorri = {
    enable = isLinux;
    enableNotifications = true;
  };

  home.packages = with pkgs; [
    ## === Uncategorized Packages ===
    hyperfine # <- Command-line benchmarking tool
    # chronologer # <- Visualize changes in benchmark timings over git history
    ccache
    pv

    ## === Local Development ===
    pkg-config # <- Manage compile and link flags for libraries
    cmake # <- Cross-platform make
    # devcontainer # <- VSCode devcontainer generator / CLI
    bc # <- Arbitrary precision calculator language
    ninja # <- Small build system with a focus on speed
    reuse # <- Tool for licensing compliance
    asciinema # <- Record and share terminal sessions
    just # <- Just a command runner
    tokei # <- Fancy wordcount for programmers
    sd # <- Modern sed replacement
    grex # <- Generate regexps from user-provided test cases.
    httpie # <- Modern, user-friendly command-line HTTP client for the API era.
    pastel # <- A command-line tool to generate, analyze, convert and manipulate colors
    hexyl # <- a command-line hex viewer
    glow # <- a markdown cli renderer (by charmbracelet)
    patchelf # <- modify the dynamic linker and RPATH of ELF executables
    fakeroot # <- Run a command in an environment faking root privileges for file manipulation
    poetry # <- sanest python package manager
    remake # <- GNU Make with comprehensible tracing and a debugger
    # config.nur.repos.foolnotion.cmake-init
    inputs'.nurpkgs.packages.sourcetrail-ng # <- Maintained fork (!!) of Sourcetrail, the best C++ exploration tool ever to live.
    terraformer # <- CLI tool to generate terraform files from existing infrastructure (reverse Terraform)

    ## === Nix Utilities ===
    nixos-rebuild
    inputs'.nvfetcher.packages.default # <- Generate nix sources expression for the latest version of packages
    nix-init # <- Generate Nix packages from URLs with hash prefetching, dependency inference, license detection, and more
    nix-template # <- Make creating nix expressions easy
    nix-diff # <- Explain why two Nix derivations differ
    nvd # <- Nix/NixOS package version diff tool
    nix-tree # <- Interactively browse dependency graphs of Nix derivations.
    nixos-option # <- Search nixos options
    nix-prefetch # <- Prefetch any fetcher function call
    nix-prefetch-git # <- Prefetch git repositories
    nurl # <- Generate  URL
    manix # <- nix documentation search
    nix-du # <- Analyze derivation and store size
    nox # <- Tools to make nix nicer to use
    zgrviewer # <- Graphviz/DOT viewer (often used with nix-du)
    nix-update # <- swiss-army knife for updating nix packages
    nix-bisect # <- Helper for bisecting nix builds
    inputs'.devenv.packages.devenv # <- Cachix's new 'devenv' tool
    vulnix # <- Scan nix (store) paths for CVEs
    nix-eval-jobs # <- Parallel nix evaluator with a streamable json output
    nixpkgs-review # <- Review nixpkgs pull requests
    nixpkgs-pytools # <- Tools for removing the tedious nature of creating nixpkgs derivations
    nix-build-uncached # <- A CI friendly wrapper around nix-build

    ## === Linters + Formatters ===
    shfmt
    shellcheck
    yamllint
    statix
    deadnix
    nixpkgs-fmt
    alejandra
    treefmt
  ] ++ (lib.optionals isLinux [
    # nixpkgs-update # <- swiss-army knife for updating nix packages
    kgraphviewer # <- KDE-flavoured Graphviz viewer
    strace # <- trace system calls and signals
    unetbootin # <- Create bootable Live USB drives for a variety of Linux distributions
  ]) ++ (lib.optionals (isLinux && !isAarch64) [
    mbuffer # tool for buffering data streams
    # postman # <- GNU Make with comprehensible tracing and a debugger
  ]) ++ (lib.optionals isDarwin [
    lorri
  ]);

  fonts.fontconfig.enable = true;

  # nix-init configuration
  xdg.configFile."nix-init/config.toml".source = toTOML "config.toml" {
    # Maintainers that will get added to the package meta
    maintainers = [ "cfeeley" ];

    # Access tokens to access private repositories and avoid rate limits
    # NOTE: just an example!
    # access-tokens = {
    #   "github.com" = "ghp_blahblahblah...";
    #   "gitlab.com".command = [ "secret-tool" "or" "whatever" "you" "use" ];
    #   "gitlab.gnome.org".file = "/path/to/api/token";
    # };
  };
  # nix-template configuration
  xdg.configFile."nix-template/config.toml".source = toTOML "config.toml" {
    # Maintainer name that will get added to the package meta
    name = "cfeeley";
  };
}
