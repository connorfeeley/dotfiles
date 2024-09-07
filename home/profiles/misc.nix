{ inputs', self', lib, pkgs, ... }: {
  # FIXME: most of these should NOT be considered part of "core"
  # most don't belong on a server, etc.
  home.packages = with pkgs; [
    ## === Uncategorized Packages ===
    just # <- Just a command runner
    speedtest-cli
    asciiquarium
    webtorrent-cli
    inputs'.nix-search-cli.packages.nix-search
    dogdns
    doggo

    ## === Data and Documents ===
    tidy-viewer # <- Pretty-print CSV files
    xsv # <- A fast CSV command line toolkit
    python3Packages.yq # <- jq wrapper for YAML/XML/TOML documents
    htmlq # <- Like jq, but for HTML
    # FIXME(2024-05-26): broken
    # csvkit # <- CSV file processing utilities
    pdftk # <- PDF toolkit
    # inputs'.nurpkgs.packages.pdftocgen # <- PDF table of contents generator

    ## === Files ===
    file
    mediainfo
    zip
    unzip
    pigz

    ## === Data Sync ===
    rclone
    inetutils # <- ftp, rcp, etc.

    ## === Repo Scripts ===
    # (lib.hiPrio n)
    # (lib.hiPrio nb)
    # nixos-rebuild-remote
    # self'.packages.nomos-rebuild
    # dotfiles-sync
    # dotfiles-push
    # dotfiles-rebuild
    # dotfiles-doom
  ] ++ (lib.optionals (pkgs.stdenv.isLinux) [
    handbrake # <- Video transcoder
    mmdoc # <- Markdown documentation generator
  ]);

  fonts.fontconfig.enable = true;

  programs.ncspot.enable = true; # Spotify ncurses client

  programs.bottom.enable = true;
  programs.eza.enable = true;
  programs.eza.enableBashIntegration = true;
  programs.eza.enableZshIntegration = true;
  programs.jq.enable = true;
  programs.pandoc.enable = true;
  programs.tealdeer.enable = true;
}
