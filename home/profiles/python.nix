{ pkgs
, ...
}: {
  home.packages = with pkgs; [
    # Overlay
    # python-env

    # lsp server
    pyright
    python3Packages.ipython
  ];
}
