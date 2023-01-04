{ peers, ... }: { self, ... }:
let
  inherit (self.inputs) digga nix-colors nixos-vscode-server nix-index-database plasma-manager;
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
      nixos-vscode-server.nixosModules.home
      nix-index-database.hmModules.nix-index
      plasma-manager.homeManagerModules.plasma-manager
      (_: { imports = [ ../lib/home ]; })
    ];

  importables = { inherit peers profiles roles; };

  users =
    let
      mkHome = name: hmArgs: {
        imports = with hmArgs.roles;
          developer ++ server ++ (with hmArgs.profiles; [ work ]);
        home = {
          username = hmArgs.lib.mkForce name;
          homeDirectory = hmArgs.lib.mkForce "/home/${name}";
          stateVersion = "22.05";
        };
      };
    in
    {
      # "cfeeley@cfeeley-laptop" = mkHome "cfeeley";
    };
}
