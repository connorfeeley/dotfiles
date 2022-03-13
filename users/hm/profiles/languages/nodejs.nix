{ config, osConfig, lib, pkgs, ... }:

let
  inherit (osConfig.my)
    email
    name
    website
    ;
in

{
  home.packages = with pkgs; [
    nodejs-16_x
    (yarn.override { nodejs = nodejs-16_x; })
  ];

  home.sessionVariables = {
    NODE_REPL_HISTORY = "${config.xdg.dataHome}/node/repl_history";
    NVM_DIR = "${config.xdg.dataHome}/node/nvm";
    NVM_AUTO_USE = "true";
    NVM_BIN = "${osConfig.environment.variables.XDG_BIN_HOME}";
    NVM_COMPLETION = "true";
    NVM_LAZY_LOAD = "true";
  };

  # npmrc *requires* that environment variables are encosed in `${...}`
  # braces, but Nix will interpret this as antiquotation within its
  # own language. For that reason, we need to escape the `$` character
  # by preceding it with double single-quotes.
  #
  # https://docs.npmjs.com/cli/v7/configuring-npm/npmrc
  # https://nixos.org/manual/nix/stable/#idm140737322046656
  xdg.configFile."npm/npmrc".text = ''
    ${lib.optionalString (email != "") "email=${email}"}
    init-license=MIT
    ${lib.optionalString (email != "") "init-author-email=${email}"}
    ${lib.optionalString (name != "") "init-author-name=${name}"}
    ${lib.optionalString (website != "") "init-author-url=${website}"}
    init-version=0.0.1
    cache=''${XDG_CACHE_HOME}/npm
    tmp=''${XDG_RUNTIME_DIR}/npm
  '';
}
