{ osConfig, config, lib, ... }:
let inherit (config.lib) dotfiles;
in {
  home.sessionVariables.AGENIX_ROOT = "${dotfiles.fsPath}/secrets/age";

  # Bitwarden CLI
  programs.rbw.enable = true;
}
