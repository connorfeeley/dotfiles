{ self, inputs, ... }:
let
  inherit (inputs)
    digga nix-colors nixos-vscode-server nix-index-database plasma-manager
    sops-nix;
  inherit (digga.lib) importExportableModules rakeLeaves;

  homeModules = importExportableModules ./modules;

  inherit (self.collective) hmArgs;

  defaultProfiles =  [ hmArgs.profiles.core ];
in
{
  imports = [ homeModules ];
  modules = defaultProfiles ++ [
    sops-nix.homeManagerModules.sops
    nix-colors.homeManagerModule
    nixos-vscode-server.nixosModules.home
    nix-index-database.hmModules.nix-index
    (_: { imports = [ ../lib/home ]; })
  ];

  users = {
    "cfeeley@cfeeley-laptop" =
      let name = "cfeeley";
      in hmArgs: {
        imports =
          (with hmArgs.roles; hmArgs.lib.flatten [ shell developer emacs-config graphical server trusted webdev fpgadev linux ]) ++
          (with hmArgs.profiles; [ shells.fish desktop.vnc ]);
        home = {
          username = hmArgs.lib.mkForce name;
          homeDirectory = hmArgs.lib.mkForce "/home/${name}";
          stateVersion = "22.05";
        };
      };
  };
}
