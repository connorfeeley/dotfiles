{ config, lib, ... }:
let inherit (config.lib) dotfield;
in {
  home.sessionVariables.AGENIX_ROOT = "${dotfield.fsPath}/secrets/age";

  # Bitwarden CLI
  programs.rbw.enable = true;
  programs.rbw.settings.email = "${lib.strings.toLower config.lib.dotfield.whoami.firstName}@${config.lib.dotfield.whoami.domain}";
}
