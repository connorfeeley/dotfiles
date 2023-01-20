{ config, lib, pkgs, ... }:
let
  inherit (config.dotfield.guardian) username;

  # Primary user's HM configuration
  guardianConfig = config.home-manager.users."${username}";

  # Emacs package selected by HM configuration
  emacsPackage = guardianConfig.programs.emacs.package;

  emacsClientPackage =
    let
      appName = "Emacs Client";

      launcher = pkgs.writeShellScript "emacsclient" ''
        ${emacsPackage}/bin/emacsclient \
            --no-wait \
            --create-frame \
            "$@"
      '';

      infoPlist =
        pkgs.writeText "Info.plist" (lib.generators.toPlist { } {
          CFBundleName = appName;
          CFBundleDisplayName = appName;
          CFBundleExecutable = "emacsclient";
          CFBundleIconFile = "Emacs";
          CFBundleIdentifier = "org.gnu.${appName}";
          CFBundleInfoDictionaryVersion = "6.0";
          CFBundlePackageType = "APPL";
          CFBundleVersion = emacsPackage.version;
          CFBundleShortVersionString = emacsPackage.version;
          CFBundleURLTypes = [
            {
              CFBundleURLName = "org-protocol handler";
              CFBundleURLSchemes = "org-protocol";
            }
          ];
        });

      icon = "${emacsPackage}/Applications/Emacs.app/Contents/Resources/Emacs.icns";
    in
    pkgs.runCommandNoCC "emacsclient-app" { } ''
      install -Dm644 "${infoPlist}" "$out/Applications/${appName}.app/Contents/Info.plist"
      install -Dm755 "${launcher}" "$out/Applications/${appName}.app/Contents/MacOS/emacsclient"
      install -Dm644 "${icon}" "$out/Applications/${appName}.app/Contents/Resources/Emacs.icns"
    '';
in
{
  environment.variables = {
    DOOMDIR = "$XDG_CONFIG_HOME/doom";
    EMACSDIR = "$XDG_CONFIG_HOME/emacs";
  };

  homebrew = {
    taps = [{ name = "railwaycat/emacsmacport"; }];
    brews = [
      # {
      #   name = "emacs-mac";
      #   args = [
      #     "with-natural-title-bar"
      #     "with-starter"
      #     # "with-mac-metal"
      #     "with-native-compilation"
      #     "with-xwidgets"
      #   ];
      # }
      # Emacs -> :lang org (macOS only)
      { name = "pngpaste"; }
      { name = "coreutils"; }
    ];
  };

  services.emacs = {
    enable = true;
    package = emacsPackage;
    additionalPath = [ "/Users/${config.dotfield.guardian.username}/.config/emacs/bin" ];
  };

  environment.systemPackages = [ emacsClientPackage ];
}
