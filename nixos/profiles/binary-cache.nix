{ inputs, config, lib, pkgs, ... }: {
  imports = [ inputs.nix-serve-ng.nixosModules.default ];
  services.nix-serve = {
    enable = true;
    secretKeyFile = "/etc/nix/cache-priv-key.pem";
  };

  systemd.services.generate-nix-cache-key = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    path = [ pkgs.nix ];
    script = ''
      [[ -f /etc/nix/cache-priv-key.pem ]] && exit
      nix-store --generate-binary-cache-key ${config.networking.hostName}-1 /etc/nix/cache-priv-key.pem /etc/nix/cache-pub-key.pem
    '';

  };
  nix.extraOptions = ''
    secret-key-files = /etc/nix/cache-priv-key.pem
  '';
}
