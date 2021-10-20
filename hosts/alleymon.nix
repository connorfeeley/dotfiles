{ config, pkgs, ... }: {
  imports = [ ../modules/darwin ];

  my = {
    username = "montchr";
    email = "chris@alley.co";
    website = "https://alley.co/";

    modules = {
      php.enable = true;
    };

    hm.accounts.email.accounts.work.primary = true;

    env = { PATH = [ "$HOME/broadway/bin" "$PATH" ]; };
  };

  networking = {
    hostName = "alleymon";

    # $ networksetup -listallnetworkservices
    knownNetworkServices = [
      "Bluetooth PAN"
      "Thunderbolt Bridge"
      "USB 10/100/1000 LAN"
      "Wi-Fi"
    ];
  };

  environment.systemPackages = with pkgs; [ dnsmasq ];

  homebrew = {
    casks = [ "figma" "microsoft-teams" "sketch" ];
    masApps = {
      "Harvest" = 506189836;
      "Jira" = 1475897096;
      "xScope" = 889428659;
      "Xcode" = 497799835;
    };
  };

  services.dnsmasq = {
    enable = false;
    addresses = {
      # Vagrant boxes.
      http = "192.168.50.4";
      test = "192.168.50.4";
    };
  };
}
