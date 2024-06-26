# SPDX-FileCopyrightText: Copyright (c) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0-or-later
#
# SPDX-FileCopyrightText: Copyright (c) 2020-2021 Jörg Thalheim and nixos-shell contributors
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/Mic92/nixos-shell/blob/55de7d4d449ff30cdde8b8fe484a86eef477245e/share/modules/nixos-shell.nix
{ config, lib, options, ... }:
let
  inherit (lib) mkEnableOption mkOption;
  inherit (lib.types) attrsOf bool raw str;
in
{
  options.nixos-vm = {
    enable = mkEnableOption "Whether to enable VM-specific configuration.";

    hostName = mkOption {
      type = str;
      default = "${config.networking.hostName}-dev";
      description = "Hostname for the virtual machine.";
    };

    remote-machine.boot.tailscaleUnlock.enable = false;

    peerConfig = mkOption {
      type = attrsOf raw;
      default = { };
      description = "Override the virtual machine's peer ops configuration.";
    };

    # FIXME: must be created manually before the VM boots / shared directories mount!
    dataHome = mkOption {
      type = str;
      # FIXME: assumes `/persist/vm` exists -- can this be stored in the
      # user's home directory or some other common path accessible to the
      # user?
      # default = "/persist/vm/${config.system.name}/data";
      # FIXME: hardcoded user path, yuck
      default = "/home/cfeeley/.local/share/vms/${config.system.name}/data";
      description =
        "Directory on the host machine where the VM's data will live.";
    };

    mounts = {
      mountHome = mkOption {
        type = bool;
        default = true;
        description = "Whether to mount <filename>/home</filename>.";
      };

      # FIXME: this won't work in pure eval mode
      mountNixProfile = mkOption {
        type = bool;
        # FIXME: should be true
        default = false;
        description = "Whether to mount the user's nix profile.";
      };

      # extraMounts = mkOption {
      #   inherit
      #     (options.virtualisation.sharedDirectories)
      #     default
      #     description
      #     example
      #     type
      #     ;
      # };
    };
  };
}
