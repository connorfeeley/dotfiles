# ##
### x86_64-linux remote builder VM hosted on local Proxmox.
### See ./install.md for the manual bootstrap procedure.
{
  config,
  lib,
  pkgs,
  primaryUser,
  modulesPath,
  ...
}:
{
  ### === networking ================================================================

  networking = {
    hostName = "proxmox-builder";
    domain = "";
    useDHCP = lib.mkDefault true;
  };

  ### === timezone =================================================================

  time.timeZone = "America/Toronto";
  environment.sessionVariables.TZ = "${config.time.timeZone}";

  ### === users ====================================================================

  dotfiles.guardian.enable = true;
  dotfiles.guardian.username = "cfeeley";

  users.mutableUsers = false;
  users.users.root.hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  users.users.cfeeley = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
    hashedPassword = "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups = [
      "wheel"
      "cfeeley"
      "secrets"
    ];
    shell = pkgs.fish;
  };

  home-manager.users = {
    cfeeley = hmArgs: {
      home.stateVersion = "23.11";
      programs.termite.enable = false;
    };
  };

  ### === services =================================================================

  services.openssh.enable = true;

  ### === nix caches ===============================================================
  # Run a local atticd backed by Cloudflare R2, and push every locally-built path
  # to it via queued-build-hook (post-build-hook). Storage lives in a
  # builder-specific R2 bucket so metadata doesn't collide with workstation's.
  nix.caches = {
    enable = true;
    attic.enable = true;
    attic.upload.enable = true;
    attic.upload.target = "local:cfeeley";
  };

  services.cache = {
    enable = true;
    enableCloudflareS3 = true;
    bucket = "cfeeley-nixpkgs-cache-builder";
    # enablePostgres left false → defaults to local sqlite at /var/lib/atticd/server.db
  };

  # cfeeley needs to be a trusted user so the Mac can substitute & build via the
  # Nix daemon over SSH. nix.sshServe handles its own key list separately
  # (see nixos/profiles/builder.nix).
  nix.settings.trusted-users = [
    "root"
    "cfeeley"
    "@wheel"
  ];

  ### === misc =====================================================================

  system.stateVersion = "23.11";

  ### === boot ====================================================================
  ### qemu-guest.nix is brought in transitively via ./hardware-configuration.nix.
  ### Bootloader config lives here because nixos-generate-config does not emit it.

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";

  imports = [ ./hardware-configuration.nix ];
}
