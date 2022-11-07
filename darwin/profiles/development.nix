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
    darwin.print-reexports
    darwin.ios-deploy
  ];

  # I'm not *quite* sure this belongs here.
  homebrew.brews = [
    "aircrack-ng" # History lesson: WEP was deprecated in 2004. No, I didn't drop a decade somewhere.
  ];
}
