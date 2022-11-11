collective @ { peers, ... }: { self, ... }:
let
  inherit (self.inputs) digga nix-colors pta2002-neovim;
  inherit (digga.lib) importExportableModules rakeLeaves;

  homeModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles { inherit profiles; };

  defaultProfiles = with profiles; [
    core # pretty hardcore bruh
  ];
in
{
  imports = [ homeModules ];
  modules =
    defaultProfiles
    ++ [
      nix-colors.homeManagerModule
      (_: { imports = [ ../lib/home ]; })
    ];

  importables = { inherit peers profiles roles; };

  users =
    let
      mkHome = name: hmArgs: {
        imports = with hmArgs.roles;
          developer ++ remote;
        home = {
          username = hmArgs.lib.mkForce name;
          homeDirectory = hmArgs.lib.mkForce "/home/${name}";
          stateVersion = "22.05";
        };
      };
    in
    {
      "cfeeley@debian-vm" = mkHome "cfeeley";
      "cfeeley@cfeeley-laptop" = mkHome "cfeeley";
    };
}
