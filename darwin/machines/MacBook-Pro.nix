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

  # Get user and group ID
  users.users.cfeeley = {
    uid = 501;
    gid = 20;
  };

  # Add 'users' group
  users.knownGroups = [ "users" ];
  users.groups.users = {
    members = [ "cfeeley" ];
    gid = 1000;
    description = "users group for syncthing compatibility";
  };

  home-manager.users = {
    cfeeley = hmArgs: {
      imports = with hmArgs.roles; workstation ++ macos ++ (with hmArgs.profiles; [
        sync
        work
      ]);

      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/Users/cfeeley";

      # Symlink Nix applications to '~/Applications/Home Manager Apps'
      # https://github.com/nix-community/home-manager/issues/1341#issuecomment-1190875080
      # https://github.com/nix-community/home-manager/pull/3139
      # FIXME: ~/Applications must be created manually first
      # NOTE: Seems to be implemented upstream - preserved for posterity.
      # home.activation = lib.mkIf pkgs.stdenv.isDarwin {
      #   copyApplications =
      #     let
      #       apps = pkgs.buildEnv {
      #         name = "home-manager-applications";
      #         paths = hmArgs.config.home.packages;
      #         pathsToLink = "/Applications";
      #       };
      #     in
      #     hmArgs.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      #       baseDir="$HOME/Applications/Home Manager Apps"
      #       if [ -d "$baseDir" ]; then
      #         rm -rf "$baseDir"
      #       fi
      #       mkdir -p "$baseDir"
      #       for appFile in ${apps}/Applications/*; do
      #         target="$baseDir/$(basename "$appFile")"
      #         $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
      #         $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
      #       done
      #     '';
      # };

      # TODO: move to/create darwin-specific home config
      # Add homebrew to PATH
      home.sessionPath = [
        (if pkgs.stdenv.hostPlatform.isAarch64 then "/opt/homebrew/bin" else "/usr/local/bin")
        (if pkgs.stdenv.hostPlatform.isAarch64 then "/opt/homebrew/sbin" else "/usr/local/sbin")
      ];
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

  programs.amphetamine = {
    enable = true;
    withEnhancer = false;
  };
}
