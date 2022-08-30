collective @ {peers, ...}: {self, ...}: let
  inherit (self.inputs) digga nix-colors;
  inherit (digga.lib) importExportableModules rakeLeaves;

  homeModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit profiles;};

  defaultProfiles = with profiles; [
    core
    direnv
    navi
    nnn
    barrier
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
      (_: {imports = [../lib/home];})
    ];

  importables = {inherit peers profiles roles;};

  users = {
    "cfeeley@cfeeley-laptop" = hmArgs: {
      imports = with hmArgs.roles;
        developer ++ remote;
      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/home/cfeeley";
    };
  };
}
