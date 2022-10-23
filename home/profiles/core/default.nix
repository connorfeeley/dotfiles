moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.home) username;
  inherit (config.lib) dotfield;
in {
  home.packages = with pkgs; [
    ## === Helpful Utilities ===
    comma #              <- Runs programs without installing them
    tealdeer #           <- A very fast implementation of tldr in Rust.
    nix-output-monitor # <- Pretty nix { build, shell, develop }

    ## === Moreutils ===
    # Includes, of note:
    # - vipe: insert a text editor into a pipe
    # - parallel: run multiple jobs at once
    # - pee: tee standard input to pipes
    # - errno: look up errno names and descriptions
    # - combine: combine the lines in two files using boolean operations
    # - ifdata: get network interface info without parsing ifconfig output
    # As well as:
    # - chronic: runs a command quietly unless it fails
    # - mispipe: pipe two commands, returning the exit status of the first
    # - ifne: run a program if the standard input is not empty
    # - isutf8: check if a file or standard input is utf-8
    # - lckdo: execute a program with a lock held
    # - sponge: soak up standard input and write to a file
    # - ts: timestamp standard input
    # - vidir: edit a directory in your text editor
    # - zrun: automatically uncompress arguments to command
    moreutils # <- Unix tools that nobody thought to write long ago when unix was young

    ## === Sysadmin ===
    du-dust #       <- Like du but more intuitive.
    dua #           <- quick disk usage
    ncdu #          <- dua with more ncurses
    entr #          <- Run arbitrary commands when files change
    lnav #          <- Log file navigator
    glances #       <- System resource usage viewer
    drill #         <- Like dig
    smartmontools # <- Mah boy's wicked smaht
    procs #         <- pspspspsps here kitty ( ps replacement )
    iotop #         <- Terminal disk IO monitor
    diskonaut #     <- Terminal visual disk space navigator

    ## === Networking Tools ===
    mosh      # <- SSH-like tool for unreliable connections
    iperf3    # <- Test network bandwidth
    bandwhich # <- Not a sandwhich; see [nethogs] ( bandwidth utilization tool )
    # TODO: not packaged
    # trippy # A network diagnostic tool

    ## === `bat` and friends ===
    # A cat(1) clone with wings.
    # (enabled via config option)
    # Bash scripts that integrate bat with various command line tools.
    # https://github.com/eth-p/bat-extras/
    bat-extras.batman #     <- Read system manual pages (man) using bat as the manual page formatter.
    bat-extras.batgrep #    <- Quickly search through and highlight files using ripgrep.
    bat-extras.batdiff #    <- Diff a file against the current git index, or display the diff between two files.
    bat-extras.batwatch #   <- Watch for changes in files or command output, and print them with bat.
    bat-extras.prettybat #  <- Pretty-print source code and highlight it with bat.

  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    kmon      # <- Kernel manager and activity monitor
    systeroid # <- A more powerful alternative to sysctl(8) with a terminal user interface
    sysz #      <- fzf-style systemd TUI
  ]);

  programs.bash.enable = true;
  programs.fish.enable = true;
  programs.zsh.enable = true;

  nix.package = pkgs.nix;

  programs.bat = {
    enable = true;
    config = {
      # No line numbers or git indicators
      style = "header";
      theme = "TwoDark";
      map-syntax = [
        ".*ignore:Git Ignore"
        ".gitconfig.local:Git Config"
        "**/mx*:Bourne Again Shell (bash)"
        "**/completions/_*:Bourne Again Shell (bash)"
        ".vimrc.local:VimL"
        "vimrc:VimL"
      ];
    };
  };

  programs.btop = {
    enable = true;
    settings = {
      vim_keys = true;
      io_mode = true;
      proc_per_core = true;
    };
  };

  programs.bottom.enable = true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.jq.enable = true;
  programs.less.enable = true;
  programs.man.enable = true;
  # N.B. This can slow down builds, but enables more manpage integrations
  # across various tools. See the home-manager manual for more info.
  programs.man.generateCaches = lib.mkDefault true;
  programs.nix-index.enable = true;
  programs.pandoc.enable = true;
  programs.tealdeer.enable = true;

  home.enableDebugInfo = true;
  home.enableNixpkgsReleaseCheck = true;
  manual.manpages.enable = true;

  home.extraOutputsToInstall = ["/share/zsh"];

  home.sessionVariables = {
    DOTFIELD_DIR = dotfield.fsPath;

    # Default is "1". But when typeset in PragmataPro that leaves no space
    # between the icon and its filename.
    # FIXME: enable when pragpro enabled
    # EXA_ICON_SPACING = "2";

    Z_DATA = "$XDG_DATA_HOME/z";
    Z_OWNER = username;

    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";

    # Docker
    DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker-machine";

    # Go
    GOPATH = "$XDG_DATA_HOME/go";

    # Rust
    CARGO_HOME = "$XDG_DATA_HOME/cargo";
    RUSTUP_HOME = "$XDG_DATA_HOME/rustup";

    # GNU screen
    SCREENRC = "$XDG_CONFIG_HOME/screen/screenrc";

    # wd
    # https://github.com/mfaerevaag/wd
    WD_CONFIG = "$XDG_CONFIG_HOME/wd/warprc";
  };

  home.stateVersion = lib.mkForce "22.05";
}
