{ inputs, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  homeManagerPackage = inputs.home-manager.packages.${system}.default;
in
{
  environment.systemPackages = [ homeManagerPackage ];

  # TODO: consider moving this to a more appropriate location
  home-manager.backupFileExtension = "backup";

  home-manager.sharedModules = [{
    programs.home-manager.enable = true;
    manual.json.enable = true;
    news.display = "show";

    # Necessary for home-manager to work with flakes, otherwise it will look for a nixpkgs channel.
    home.stateVersion = "21.11"; # Version of *original* HM state.

    xdg.enable = true;
  }];
}
