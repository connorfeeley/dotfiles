{ lib, pkgs, ... }:
let
  extensions = with pkgs.vscode-extensions;
    [
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
      ms-python.python
      ms-pyright.pyright

      # Markdown
      yzhang.markdown-all-in-one
    ] ++ lib.optionals (pkgs.stdenv.isLinux && pkgs.stdenv.isx86_64) [
      ms-vscode.cpptools
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      ###
      ### Extra extensions from the marketplace (not available in nixpkgs)
      ###
      # Talon voice
      {
        # https://marketplace.visualstudio.com/_apis/public/gallery/publishers/pokey/vsextensions/talon/0.2.0/vspackage
        name = "talon";
        publisher = "pokey";
        version = "0.2.0";
        sha256 = "sha256-BPc0jGGoKctANP4m305hoG9dgrhjxZtFdCdkTeWh/Xk=";
      }
      {
        # https://marketplace.visualstudio.com/_apis/public/gallery/publishers/pokey/vsextensions/talon/0.2.0/vspackage
        name = "cursorless";
        publisher = "pokey";
        version = "0.26.590";
        sha256 = "sha256-hYeUaiUJa/PzpPobapD4+ppaRH+n0E8EAw9swJ0ZaVk=";
      }
      {
        # https://marketplace.visualstudio.com/_apis/public/gallery/publishers/pokey/vsextensions/talon/0.2.0/vspackage
        name = "parse-tree";
        publisher = "pokey";
        version = "0.25.0";
        sha256 = "sha256-MCRLCTAybBL7W85QedJmXi2ZJvOJa13oaUtoJwCJx7U=";
      }
      {
        # https://marketplace.visualstudio.com/_apis/public/gallery/publishers/pokey/vsextensions/talon/0.2.0/vspackage
        name = "vscode-talonscript";
        publisher = "mrob95";
        version = "0.3.14";
        sha256 = "sha256-7obctmjguR5la5omzjO07pofTZ3t7pXq8jxAJ9GPq5M=";
      }

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
  vscodeFhs = pkgs.vscode.fhsWithPackages
    (ps: with ps; [ rustup zlib openssl.dev pkg-config ]);
in
{
  programs.vscode = {
    enable = true;
    inherit extensions;

    # Add extension-specific dependencies needed for rust lang server and rust-analyzer extension
    package = lib.mkIf false vscodeFhs;
  };
}
