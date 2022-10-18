collective @ {peers, ...}: {self, ...}: let
  inherit (self.inputs) digga nix-colors pta2002-neovim;
  inherit (digga.lib) importExportableModules rakeLeaves;

  homeModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit profiles;};

  defaultProfiles = with profiles; [
    core
    direnv
    nnn
    ranger
    secrets.common
    tealdeer
    vim
  ];
in {
  imports = [homeModules];
  modules =
    defaultProfiles
    ++ [
      nix-colors.homeManagerModule
      pta2002-neovim.homeManagerModules.nixvim
      (_: {imports = [../lib/home];})
    ];

  importables = {inherit peers profiles roles;};

  users = {
    "cfeeley@debian-vm" = hmArgs: {
      imports = with hmArgs.roles;
        developer ++ remote ++ generic-linux;
      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/home/cfeeley";
    };
    "cfeeley@cfeeley-laptop" = hmArgs: {
      imports = with hmArgs.roles;
        developer ++ remote ++ generic-linux;
      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/home/cfeeley";
    };
  };
}
