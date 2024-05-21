{ pkgs, self', ... }: {
  environment.systemPackages = with pkgs; [
    self'.packages.mints

    darwin.DarwinTools
    darwin.print-reexports
    undmg
    darwin.ios-deploy
  ];

  homebrew = {
    taps = [{
      name = "blacktop/tap";
    } # ipsw
    ];
    brews = [
      # I'm not *quite* sure this belongs here.
      {
        name = "aircrack-ng";
      } # History lesson: WEP was deprecated in 2004. No, I didn't drop a decade somewhere.

      { name = "ipsw"; } # <- iOS/macOS Research Swiss Army Knife
    ];
    casks = [{ name = "altserver"; } { name = "github"; }];
  };
}
