{ config
, lib
, pkgs
, ...
}:
let
  # Selector flag for xquartz package source: homebrew vs nixpkgs
  fromBrew = true;

  safariIconPath = "/Applications/Safari.app/Contents/Resources/AppIcon.icns";
  orionIconPath = "/Applications/Orion.app/Contents/Resources/AppIcon.icns";
  setAppIcon = pkgs.writeShellScriptBin "orion-set-app-icon" ''
    echo "Overwriting Orion.app icon with Safari icon..."
    cp ${safariIconPath} ${orionIconPath}
    rm /var/folders/*/*/*/com.apple.dock.iconcache
    killall Dock
    echo -n " done"
  '';
in
{
  ###
  ### From nixpkgs
  ###
  environment.systemPackages = [ setAppIcon ] ++ lib.optionals (!fromBrew) (with pkgs; [
    xquartz # Run 'xquartz-install' after installation
  ]);

  ###
  ### From homebrew
  ###
  homebrew.casks = lib.mkIf fromBrew [
    { name = "orion"; } # Native WebKit-based browser
  ];

  # Post-user activation script is run directly after homebrew activation
  system.activationScripts.postUserActivation.text = ''
    # Enable Orion debug menu
    defaults write com.kagi.kagimacOS DebugMenu 1
  '';
}
