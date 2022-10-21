{ config
, lib
, pkgs
, profiles
, suites
, inputs
, primaryUser
, collective
, ...
}:
{
  environment.systemPackages = with pkgs; [
    darwin.DarwinTools
    darwin.ios-deploy
  ];
}
