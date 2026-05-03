{ inputs, lib, pkgs, ... }:
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

    # Backstop only — per-machine configs override. Was plain '=' previously,
    # which conflicted with machine settings; mkDefault makes it a real fallback.
    home.stateVersion = lib.mkDefault "21.11";

    xdg.enable = true;
  }];
}
