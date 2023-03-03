{ pkgs, ... }: {
  home.packages = with pkgs;
    [
      # Overlay
      # python-env

      # lsp server
      pyright
      openai
    ] ++ (with python3Packages; [ ipython black ]);
}
