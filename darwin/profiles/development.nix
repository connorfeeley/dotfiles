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

  homebrew = {
    taps = [
      "blacktop/tap" # ipsw
    ];
    brews = [
      # I'm not *quite* sure this belongs here.
      "aircrack-ng" # History lesson: WEP was deprecated in 2004. No, I didn't drop a decade somewhere.

      "ipsw" # <- iOS/macOS Research Swiss Army Knife
    ];
  };
}
