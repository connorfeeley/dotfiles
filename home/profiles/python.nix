{ pkgs, ... }: {
  home.packages = with pkgs;
    [
      # Overlay
      # python-env

      # lsp server
      pyright
      openai
      # (openai-whisper.override ({ torch = pkgs.python3.pkgs.torchWithCuda; }))
    ] ++ (with python3Packages; [ ipython black ]);
}
