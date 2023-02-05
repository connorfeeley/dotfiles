{ pkgs
, ...
}: {
  home.packages = with pkgs; [
    # Overlay
    # python-env

    # lsp server
    pyright
  ] ++ (with python3Packages; [
    ipython
    black
  ]);
}
