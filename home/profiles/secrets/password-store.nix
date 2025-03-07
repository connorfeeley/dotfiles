{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config.lib.dotfiles.whoami) pgpPublicKey;
in
lib.mkMerge [
  {
    home.packages = [ 
      # pkgs.ripasso-cursive
      pkgs.qtpass 
    ];
    programs.password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [
        exts.pass-otp
        exts.pass-audit
        exts.pass-update
        exts.pass-checkup
        exts.pass-genphrase
      ] ++ (if isDarwin then [ ] else [ exts.pass-import ]));
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
        PASSWORD_STORE_KEY = pgpPublicKey;
      };
    };
  }
  (lib.mkIf (!isDarwin) {
    # services.pass-secret-service.enable = true;
  })
  (lib.mkIf config.programs.firefox.enable {
    programs.browserpass.enable = true;
    programs.browserpass.browsers = [ "firefox" ];
  })
]
