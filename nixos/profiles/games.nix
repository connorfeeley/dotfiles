{ pkgs
, ...
}:
{
  environment.systemPackages = with pkgs; [
    minecraft

    # CLI program for managing Minecraft modpacks from Modrinth, CurseForge, and Github Releases
    ferium
  ];

  programs.steam.enable = true;
}
