{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.programs.amphetamine;
in
{
  options.programs.amphetamine = {
    enable = lib.mkEnableOption
      ''Whether Amphetamine should be installed (from the App Store; must have installed it with your Apple ID at least once).'';
    withEnhancer = lib.mkEnableOption "Whether the Amphetamine Enhancer application should be installed.";
  };

  config = lib.mkMerge ([
    (lib.mkIf cfg.enable {
      homebrew.masApps = {
        "Amphetamine" = 937984704;
      };
    })

    (lib.mkIf cfg.withEnhancer {
      environment.systemPackages = with pkgs; [
        amphetamine-enhancer
      ];
    })
  ]);
}
