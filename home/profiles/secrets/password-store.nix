{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config.lib.dotfield.whoami) pgpPublicKey;

  passWithExtensions = pkgs.pass.withExtensions (exts:
    with exts; [
      pass-import # https://github.com/roddhjav/pass-import
      pass-otp # https://github.com/tadfisher/pass-otp
      pass-update # https://github.com/roddhjav/pass-update
    ]);
in
lib.mkMerge [
  {
    programs.password-store = lib.mkIf config.programs.gpg.enable {
      enable = true;
      package = pkgs.gopass; # Or passWithExtensions
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
        PASSWORD_STORE_KEY = pgpPublicKey;
      };
    };
  }
  (lib.mkIf (!isDarwin) {
    services.pass-secret-service.enable = true;
    services.password-store-sync.enable = true;
  })
  (lib.mkIf config.programs.firefox.enable {
    programs.browserpass.enable = true;
    programs.browserpass.browsers = [ "firefox" ];
  })
]
