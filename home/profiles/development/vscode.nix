{ lib
, pkgs
, ...
}:
let
  extensions = with pkgs.vscode-extensions; [
    ### Visuals
    dracula-theme.theme-dracula

    ### Tools
    github.copilot
    eamodio.gitlens
    ms-vscode-remote.remote-ssh
    ms-azuretools.vscode-docker

    ### Keybindings
    vscodevim.vim

    ### Environment
    mkhl.direnv
    arrterian.nix-env-selector

    ### Langs
    # Nix
    jnoortheen.nix-ide

    # C++
    ms-vscode.cpptools
    ms-vscode.cmake-tools

    # Shell
    mads-hartmann.bash-ide-vscode
    timonwong.shellcheck

    # Go
    golang.go

    # Haskell
    haskell.haskell

    # Python
    ms-python.python
    ms-pyright.pyright

    # Markdown
    yzhang.markdown-all-in-one
  ];

  # With extension-specific dependencies, to be added to the FHS environment
  vscodeFhs = pkgs.vscode.fhsWithPackages (ps: with ps; [
    rustup
    zlib
    openssl.dev
    pkg-config
  ]);
in
{
  programs.vscode = {
    enable = true;
    inherit extensions;

    # Add extension-specific dependencies needed for rust lang server and rust-analyzer extension
    # package = vscodeFhs;
  };
}