{ lib
, config
, ...
}:
let

  inherit (config.lib) dotfield;
  inherit (config.dotfield.guardian) username;
  configDir = "${dotfield.userConfigPath}/hammerspoon";
in
{
  # homebrew.casks = [{ name = "hammerspoon"; }];
  services.hammerspoon.enable = true;
  services.hammerspoon.spoons = [
    { name = "AClock"; }
    { name = "Cherry"; }
  ];
  services.hammerspoon.configDirectory = config.home-manager.users."${username}".home.homeDirectory + "/.hammerspoon";

  # Point Hammerspoon to its init file.
  # https://github.com/Hammerspoon/hammerspoon/pull/582
  system.activationScripts.postUserActivation.text = ''
    defaults write org.hammerspoon.Hammerspoon MJConfigFile "${configDir}/init.lua"
  '';
}
