{ pkgs, ... }: {
  home.packages = [
    # General python environment
    (pkgs.python3.withPackages (ps: with ps; [ ipython pip black pyflakes isort ]))

    # LSP server
    pkgs.pyright

    pkgs.openai
    # (openai-whisper.override ({ torch = pkgs.python3.pkgs.torchWithCuda; }))
  ];
}
