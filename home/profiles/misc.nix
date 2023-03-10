{ lib, pkgs, ... }: {
  # FIXME: most of these should NOT be considered part of "core"
  # most don't belong on a server, etc.
  home.packages = with pkgs;
    [
      ## === Uncategorized Packages ===
      just # <- Just a command runner
      speedtest-cli
      asciiquarium

      ## === Data and Documents ===
      tidy-viewer # <- Pretty-print CSV files
      xsv # <- A fast CSV command line toolkit
      python3Packages.yq # <- jq wrapper for YAML/XML/TOML documents
      htmlq # <- Like jq, but for HTML

      ## === Media Tools ===
      # TODO: media should be it's own profile; +use hm module
      ffmpeg-full # <- Full ffmpeg with all codecs
      chafa # <- "terminal graphics for the 21st century"
      yt-dlp # <- youtube-dl fork
      mpv

      ## === Files ===
      file
      mediainfo
      unzip
      pigz

      ## === Data Sync ===
      rclone

      ## === Repo Scripts ===
      nixos-rebuild-remote
      dotfield-sync
      dotfield-push
      dotfield-rebuild
      dotfield-doom
    ] ++ (lib.optionals (pkgs.stdenv.isLinux) [
      handbrake # <- Video transcoder
    ]);

  fonts.fontconfig.enable = true;

  # TODO: check these out
  # services.etebase = {};
  # services.etebase.sync = {};
  # services.flameshot = {};
  # services.hound = ...
  # programs.ncspot = {}; # spotify thingy

  programs.bottom.enable = true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.jq.enable = true;
  programs.pandoc.enable = true;
  programs.tealdeer.enable = true;
}
