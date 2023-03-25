{ config, lib, pkgs, ... }:
let
  inherit (config.lib.dotfield.whoami) domain;
  inherit (config.lib.dotfield.secrets) mkAgeSecret;
  inherit (config.dotfield) guardian;
in
{
  #
  age.secrets = lib.mkMerge [ (mkAgeSecret "fastmail.txt") ];
  programs.msmtp = {
    enable = true;
    setSendmail = true;
    accounts.fastmail = {
      auth = true;
      tls = true;
      tls_starttls = false;
      port = 465;
      from = "automated@cfeeley.org";
      host = "mail.messagingengine.com";
      passwordeval = "cat ${config.age.secrets."fastmail.txt".path}";
      user = "connor@cfeeley.org";
    };
    extraConfig = ''
      account default : fastmail
    '';
    defaults.aliases = "/etc/aliases";
  };

  environment.etc."aliases" = {
    text = ''
      root: automated+root@cfeeley.org
      cfeeley: automated+cfeeley@cfeeley.org
    '';
    mode = "0644";
  };

  users.users.${guardian.username}.extraGroups = [ "mail" ];
}
