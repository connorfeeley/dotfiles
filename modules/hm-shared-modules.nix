{ inputs, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  homeManagerPackage = inputs.home-manager.packages.${system}.default;
in
{
  environment.systemPackages = [ homeManagerPackage ];

  # TODO: right location for this?
  home-manager.backupFileExtension = "backup";

  home-manager.sharedModules = [{
    programs.home-manager.enable = true;
    manual.json.enable = !pkgs.stdenv.isDarwin; # FIXME
    news.display = "show";

    # Necessary for home-manager to work with flakes, otherwise it will look for a nixpkgs channel.
    home.stateVersion = "21.11"; # Version of *original* HM state.

    xdg.enable = true;

    # TODO: what benefit does symlinking this provide?
    # xdg.configFile."nix/registry.json".text =
    #   config.environment.etc."nix/registry.json".text;
  }];
}
