{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.lib.our) treesWithEnabledLeaf;

  /*
    hasEnabledModule :: [String] -> AttrSet -> Bool

    Whether any home-manager user on the system has enabled the module at
    the given path.

    Example:

    ```
    {
    home-manager.users = {
      foo = { programs.emacs.enable = true; };
      bar = { programs.neovim.enable = true; };
    };
    programs.emacs.enable = hasEnabledModule ["programs" "emacs"];
    }
    ```
  */
  hasEnabledModule = path:
    let
      trees = treesWithEnabledLeaf (path ++ [ "enable" ]) config.home-manager.users;
    in
    ((builtins.length trees) >= 1);

  /*
    hasWm :: String -> Bool

    Whether the given window manager is enabled by any user.
  */
  hasWm = name: hasEnabledModule [ "wayland" "windowManager" name ];

  defaultUsername = "cfeeley";
in
{
  lib.dotfield = rec {
    srcPath = toString ../../.;
    fsPath = "/etc/dotfield";
    userConfigPath = toString "${srcPath}/home/users/${defaultUsername}/config";

    secrets = rec {
      # nix-darwin does not support the `users.<name>.extraGroups` option, but
      # that's not a problem since we're only using darwin systems as a single
      # admin user. although the username may vary across systems, each "primary
      # user" will still be in the `admin` group.
      secretsGroup =
        if pkgs.stdenv.isLinux
        then "secrets"
        else "admin";
      secretsDir = toString ../../secrets;
      mkAgeSecret = name: {
        "${name}" = {
          file = "${secretsDir}/${name}.age";
          group = secretsGroup;
          mode = "770";
        };
      };
    };

    sys = {
      # Whether a NixOS system has enabled the proprietary NVIDIA drivers.
      #
      # FIXME: The default `false` value indicates that we cannot know with
      # certainty whether NVIDIA drives are in use. This may be the case, for
      # example, on generic Linux with a standalone home-manager.
      hasNvidia =
        if pkgs.stdenv.isLinux
        then (config.hardware.nvidia.package != null)
        else false;

      # Whether a tiling window manager is enabled system-wide.
      hasTwm =
        config.services.yabai.enable
          or (pkgs.stdenv.isLinux ? config.programs.sway.enable);

      # Whether the system has any features indicating a Wayland session.
      hasWayland =
        config.services.xserver.displayManager.gdm.wayland
          or (pkgs.stdenv.isLinux ? config.programs.sway.enable);
    };

    home = { inherit userConfigPath hasEnabledModule hasWm; };
  };
}
