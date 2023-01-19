{ pkgs
, ...
}:
{
  environment.systemPackages = with pkgs; [
    darwin.DarwinTools
    darwin.print-reexports
    undmg
    # FIXME(darwin): build failing as of 2022-12-13
    # darwin.ios-deploy
  ];

  homebrew = {
    taps = [
      { name = "blacktop/tap"; } # ipsw
    ];
    brews = [
      # I'm not *quite* sure this belongs here.
      { name = "aircrack-ng"; } # History lesson: WEP was deprecated in 2004. No, I didn't drop a decade somewhere.

      { name = "ipsw"; } # <- iOS/macOS Research Swiss Army Knife
    ];
    casks = [
      { name = "altserver"; }
    ];
  };
}
