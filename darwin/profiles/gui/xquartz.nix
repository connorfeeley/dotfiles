{ lib
, pkgs
, ...
}:
let
  # Selector flag for xquartz package source: homebrew vs nixpkgs
  fromBrew = true;
in
{
  ###
  ### From nixpkgs
  ###
  environment.systemPackages = lib.mkIf (!fromBrew) (with pkgs; [
    xquartz # Run 'xquartz-install' after installation
  ]);

  ###
  ### From homebrew
  ###
  homebrew.casks = lib.mkIf fromBrew [
    { name = "xquartz"; }
  ];
}
