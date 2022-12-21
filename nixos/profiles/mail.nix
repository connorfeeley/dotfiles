{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.lib.dotfield.whoami) domain;
  inherit (config.lib.dotfield.secrets) mkAgeSecret;
in
{
  age.secrets = lib.mkMerge [
    (mkAgeSecret "fastmail.txt")
  ];
  programs.msmtp = {
    enable = true;
    setSendmail = true;
    accounts.default = {
      auth = true;
      tls = true;
      from = "automated@cfeeley.org";
      host = "mail.messagingengine.com";
      passwordeval = "cat ${config.age.secrets."fastmail.txt".path}";
      user = "cfeeley.org";
    };
  };
}
