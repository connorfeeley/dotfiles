{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.chromium = {
    enable = true;
    # Extensions don't get installed with ungoogled-chromium, so I guess I'll use chromium.
    package = pkgs.chromium;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
      { id = "nngceckbapebfimnlniiiahkandclblb"; } # bitwarden
      { id = "hfjbmagddngcpeloejdejnfgbamkjaeg"; } # vimium-c
      { id = "lanfdkkpgfjfdikkncbnojekcppdebfp"; } # canvas fingerprint defend
      { id = "fhkphphbadjkepgfljndicmgdlndmoke"; } # font fingerprint defend
      { id = "olnbjpaejebpnokblkepbphhembdicik"; } # webgl fingerprint defend
      { id = "pcbjiidheaempljdefbdplebgdgpjcbe"; } # audio fingerprint defend
      { id = "dhdgffkkebhmkfjojejmpbldmpobfkfo"; } # tampermonkey
      {
        # chromium web store
        id = "ocaahdebbfolfmndjeplogmgcagdmblk";
        crxPath = builtins.fetchurl {
          name = "chromium-web-store.crx";
          url = "https://github.com/NeverDecaf/chromium-web-store/releases/download/v1.5.4.2/Chromium.Web.Store.crx";
          sha256 = "sha256:0q3js6r6wzy0hqdjgm9n8kmwb8hn6prap7gp3vx0z3xgipgpp92c";
        };
        version = "1.5.4.2";
      }
    ];
  };
}
