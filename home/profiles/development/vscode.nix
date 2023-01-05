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
    ms-vscode.cmake-tools

    # Shell
    mads-hartmann.bash-ide-vscode
    timonwong.shellcheck

    # Go
    golang.go

    # Haskell
    haskell.haskell

    # Python
    ms-pyright.pyright

    # Markdown
    yzhang.markdown-all-in-one
  ] ++ lib.optionals (pkgs.stdenv.isLinux && pkgs.stdenv.isx86_64) [
    ms-vscode.cpptools
    ms-python.python
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    ###
    ### Extra extensions from the marketplace (not available in nixpkgs)
    ###
    # Verilog
    {
      name = "svls-vscode";
      publisher = "dalance";
      version = "0.0.3";
      sha256 = "sha256-lokp6NPPfw8/Ptp5SujX8Wo13W1hYdG5s21wBLpj4N8=";
    }
    {
      name = "svlangserver";
      publisher = "imctradingbv";
      version = "0.4.1";
      sha256 = "sha256-+WfgVMSFncYg/EeaEFXPxomummTqVsyRPEw6wIura84=";
    }
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
    package = lib.mkIf false vscodeFhs;
  };
}
