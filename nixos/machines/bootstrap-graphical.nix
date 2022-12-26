{ ...
}: {
  system.stateVersion = "22.11";

  boot.loader.systemd-boot.enable = true;

  # Will be overridden by the bootstrapIso module.
  fileSystems."/" = { device = "/dev/disk/by-label/nixos"; };

  users.users.cfeeley = {
    password = "cfeeley";
    description = "default";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  home-manager.users.cfeeley = hmArgs: {
    imports =
      (with hmArgs.roles; graphical) ++
      (with hmArgs.profiles; [
        # desktop.xmonad
      ]);
  };
}
