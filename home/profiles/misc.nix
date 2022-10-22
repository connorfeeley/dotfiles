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

    ## === Data and Documents ===
    tidy-viewer # Pretty-print CSV files
    xsv # A fast CSV command line toolkit
    python3Packages.yq # <- jq wrapper for YAML/XML/TOML documents

    ## === Media Tools ===
    # TODO: media should be it's own profile; +use hm module
    chafa #    <- "terminal graphics for the 21st century"
    mpv
    youtube-dl

    ## === Files ===
    file
    mediainfo
    unzip

    ## === Data Sync ===
    rclone
  ] ++ (lib.optionals pkgs.stdenv.isLinux [ ]);

  fonts.fontconfig.enable = true;

  # TODO: check these out
  # services.etebase = {};
  # services.etebase.sync = {};
  # services.flameshot = {};
  # services.hound = ...
  # programs.ncspot = {}; # spotify thingy
}
