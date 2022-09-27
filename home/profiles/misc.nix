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
    comma
    hyperfine
    mosh
    barrier

    ## === System Tools ===
    iotop
    smartmontools
    diskonaut # Terminal visual disk space navigator

    ## === Networking Tools ===
    iperf3
    bandwhich # Terminal bandwidth utilization tool
    # FIXME: not packaged
    # trippy # A network diagnostic tool

    ## === Local Development ===

    act # Run GitHub Actions locally
    asciinema
    circleci-cli
    universal-ctags
    just # Just a command runner
    tokei # Fancy wordcount for programmers

    ## === Nix Development ===
    fup-repl

    nickel

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
    deadnix
    nixfmt

    ## === Media Tools ===

    # TODO: use hm module
    mpv
    youtube-dl

    ## === Data Sync ===
    rclone
  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    sourcetrail # Rest in peace sourcetrail, the best C++ exploration tool ever to live.
    kmon # Kernel manager and activity monitor
    systeroid # A more powerful alternative to sysctl(8) with a terminal user interface
    procs # pspspspsps
    sysz  # fzf-style systemd TUI
  ]);

  # TODO: check these out
  # services.etebase = {};
  # services.etebase.sync = {};
  # services.flameshot = {};
  # services.hound = ...
  # programs.ncspot = {}; # spotify thingy
}
