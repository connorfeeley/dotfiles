{ config
, lib
, ...
}:
let
  inherit (config.lib) dotfield;
in
{
  home.sessionVariables.AGENIX_ROOT = "${dotfield.fsPath}/secrets/age";
}
