{ config, pkgs, ... }: {
  imports = [
    ../modules/darwin
    ../modules/php.nix
  ];

  my = {
    username = "montchr";
    email = "chris@alley.co";
    website = "https://alley.co/";

    modules = {
      php.enable = true;
    };

    env = {
      PATH = [ "$HOME/broadway/bin" "$PATH" ];
    };
  };

  networking.hostName = "alleymon";

  environment.systemPackages = with pkgs; [
    dnsmasq
  ];

  services.dnsmasq = {
    enable = true;
    addresses = {
      # Vagrant boxes.
      http = "192.168.50.4";
      test = "192.168.50.4";
    };
  };
}
