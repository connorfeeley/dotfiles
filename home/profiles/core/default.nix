moduleArgs @ { config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isAarch64;

  inherit (config.home) username;
  inherit (config.lib) dotfield;

  hasNvidia = moduleArgs.osConfig.lib.dotfield.sys.hasNvidia or false;

  zenithPackage = with pkgs;
    if hasNvidia
    then zenith-nvidia
    else zenith;
in
{
  home.packages = with pkgs; [
    ## === Helpful Utilities ===
    comma #              <- Runs programs without installing them
    tealdeer #           <- A very fast implementation of tldr in Rust.
    nix-output-monitor # <- Pretty nix { build, shell, develop }
    thefuck #            <- Corrects your previous console command
    mtr #                <- Traceroute and ping in a single tool

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
    duf #           <- df with colours
    dua #           <- quick disk usage
    entr #          <- Run arbitrary commands when files change
    lnav #          <- Log file navigator
    glances #       <- System resource usage viewer
    ldns #          <- 'drill' (like dig)
    smartmontools # <- Mah boy's wicked smaht
    procs #         <- pspspspsps here kitty ( ps replacement )
    diskonaut #     <- Terminal visual disk space navigator

    ## === Networking Tools ===
    mosh # <- SSH-like tool for unreliable connections
    iperf3 # <- Test network bandwidth
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
    ncdu #          <- dua with more ncurses
    iotop #         <- Terminal disk IO monitor
    psmisc #        <- Useful utilities that use the proc filesystem
    kmon #          <- Kernel manager and activity monitor
    systeroid #     <- A more powerful alternative to sysctl(8) with a terminal user interface
    sysz #          <- fzf-style systemd TUI
  ]) ++ (lib.optionals (!isAarch64) [
    zenithPackage # <- Other system resource usage viewer
  ]);

  # Configure 'thefuck'
  programs.bash.initExtra = "eval $(thefuck --alias)";
  programs.zsh.initExtra = "eval $(thefuck --alias)";
  programs.fish.interactiveShellInit = "thefuck --alias | source";

  nix.package = lib.mkDefault pkgs.nix;

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

  programs.less.enable = true;
  programs.man.enable = true;
  programs.info.enable = true;
  # N.B. This can slow down builds, but enables more manpage integrations
  # across various tools. See the home-manager manual for more info.
  programs.man.generateCaches = lib.mkDefault true;
  programs.nix-index.enable = true;

  home.enableDebugInfo = true;
  home.enableNixpkgsReleaseCheck = true;

  manual = {
    manpages.enable = true;
    html.enable = true;
    json.enable = true;
  };

  home.extraOutputsToInstall = [ "/share/zsh" ];

  home.sessionVariables = {
    DOTFIELD_DIR = "$XDG_CONFIG_HOME/dotfield";

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

  home.stateVersion = lib.mkDefault "22.05";
}
