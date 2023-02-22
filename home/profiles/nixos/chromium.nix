{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.chromium = {
    enable = true;
    package = pkgs.microsoft-edge;
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
          url = "https://github.com/NeverDecaf/chromium-web-store/releases/download/v1.4.0/Chromium.Web.Store.crx";
          sha256 = "1bfzd02a9krkapkbj51kxfp4a1q5x2m2pz5kv98ywfcarbivskgs";
        };
        version = "1.4.0";
      }
    ];
  };
}
