{ config
, lib
, pkgs
, profiles
, suites
, inputs
, primaryUser
, collective
, ...
}:
let
  inherit (collective) peers;
  inherit (config.networking) hostName;
in
{
  ### === users ================================================================

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cfeeley";

  home-manager.users = {
    cfeeley = hmArgs: {
      imports = with hmArgs.roles; workstation;

      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/Users/cfeeley";

      # Symlink Nix applications to '~/Applications/Home Manager Apps'
      # https://github.com/nix-community/home-manager/issues/1341#issuecomment-1190875080
      # https://github.com/nix-community/home-manager/pull/3139
      # FIXME: ~/Applications must be created manually first
      home.activation = lib.mkIf pkgs.stdenv.isDarwin {
        copyApplications =
          let
            apps = pkgs.buildEnv {
              name = "home-manager-applications";
              paths = hmArgs.config.home.packages;
              pathsToLink = "/Applications";
            };
          in
          hmArgs.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            baseDir="$HOME/Applications/Home Manager Apps"
            if [ -d "$baseDir" ]; then
              rm -rf "$baseDir"
            fi
            mkdir -p "$baseDir"
            for appFile in ${apps}/Applications/*; do
              target="$baseDir/$(basename "$appFile")"
              $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
              $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
            done
          '';
      };
    };
  };

  networking.hostName = "MacBook-Pro";

  # FIXME: verify
  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Wi-Fi"
    "iPhone USB"
    "Thunderbolt Bridge"
    "Tailscale Tunnel"
  ];
}