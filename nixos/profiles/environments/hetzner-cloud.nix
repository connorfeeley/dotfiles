{ config
, lib
, primaryUser
, modulesPath
, pkgs
, ...
}:
let
  inherit (lib)
    mkOption
    mkDefault
    mkForce
    types
    ;
in
{
  options = {
    environments.isHetzner = mkOption {
      type = types.bool;
      description = "R/O (ish) flag indicating to home-manager that the target environment is a Hetzner VM.";
      default = true;
      readOnly = true;
    };
  };

  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  config = {
    boot.cleanTmpDir = true;
    zramSwap.enable = true;
    networking.hostName = "h8tsner";
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBxw8UnnH5Cizu7p9r4PFGDe/azUrdC0qA3K9GtWtvf/+l4dy044X3mI+hHVigTbxDH5viYcTiH6Lk+SHl2uZuX6fkzTBaFoonEJrKeCRS25TTMmas9g7D/maDoENEF1X0acs5Ffk3CAqKlOeynGPnj4M1ovUM8wyg1lsfZXA+LVr9GLLziiZSxVBBjG341hfVP3LFijj8qIAoDnBPrlLBjrrCsHXZa1QxjjyQADC5Ty7wgqLZqhfEEmkSdUEdkEt1lW4wzJzNXM/7F+iBmLTTp2KcUTPP2kyCU8YR+QvOMafB7ufmRoMf2ERjQtCwSJCYfEot3DBOvdgL0lFBTW4T /Users/cfeeley/.ssh/id_rsa"
    ];

    boot.loader.grub.device = "/dev/sda";
    boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "vmw_pvscsi" "xen_blkfront" ];
    boot.initrd.kernelModules = [ "nvme" ];
    fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };

    services.openssh = lib.mkDefault {
      enable = true;
      openFirewall = true;
      # Authorized keys and permitRootLogin are set in ssh-host profile
    };
  };
}
