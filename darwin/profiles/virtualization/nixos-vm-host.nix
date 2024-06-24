{ self, lib, inputs', pkgs, ... }:
let
  builder-service = "system/org.nixos.linux-builder";
  restartBuilderScript = pkgs.writeShellScriptBin "restart-builder" ''
    sudo launchctl kickstart -k ${builder-service}
  '';
  stopBuilderScript = pkgs.writeShellScriptBin "stop-builder" ''
    sudo launchctl stop ${builder-service}
  '';
  startBuilderScript = pkgs.writeShellScriptBin "start-builder" ''
    sudo launchctl kickstart -k ${builder-service}
  '';
in
{
  nix.settings = {
    builders = lib.mkForce [
      "@/etc/nix/machines"
      # "ssh-ng://builder@linux-builder aarch64-linux /etc/nix/builder_ed25519 8 - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo="
    ];
    builders-use-substitutes = true;
  };

  launchd.daemons.linux-builder = {
    serviceConfig = {
      StandardOutPath = "/var/log/darwin-builder.log";
      StandardErrorPath = "/var/log/darwin-builder.log";
    };
  };

  # Must copy SSH config file (created by linux-builder module) otherwise SSH complains about permissions (on the symlink).
  environment.etc."ssh/ssh_config.d/100-linux-builder.conf".copy = true;

  nix.linux-builder = {
    enable = true;
    package = inputs'.nixos-stable-darwin.legacyPackages.darwin.linux-builder; # Use image from stable
    maxJobs = 8;
    speedFactor = 8 * 2;
    ephemeral = true;

    # Extra config for builder.
    config = ({ pkgs, ... }: {
      environment.systemPackages = [ pkgs.nixos-rebuild pkgs.btop pkgs.duf ];

      virtualisation.darwin-builder.diskSize = 60 * 1024;
      virtualisation.darwin-builder.memorySize = 4096 * 2;
      virtualisation.cores = 8;

      users.users.builder.openssh.authorizedKeys.keys = (self.collective.peers.hosts.MacBook-Pro).keys;
      services.openssh.enable = true;
      programs.ssh.extraConfig = ''
        Host bikes.cfeeley.org
            User root
            Port 26473
            IdentityFile ~/.config/ssh/tailscale-haskbike-bridge.pem
      '';
    });
  };

  environment.systemPackages = [ restartBuilderScript stopBuilderScript startBuilderScript ];
}
