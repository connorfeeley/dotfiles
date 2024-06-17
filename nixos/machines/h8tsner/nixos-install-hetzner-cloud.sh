#!/usr/bin/env bash

# Script to install NixOS from the Hetzner Cloud NixOS bootable ISO image.
# (tested with Hetzner's `NixOS 20.03 (amd64/minimal)` ISO image).
#
# This script wipes the disk of the server!
#
# Instructions:
#
# 1. Mount the above mentioned ISO image from the Hetzner Cloud GUI
#    and reboot the server into it; do not run the default system (e.g. Ubuntu).
# 2. To be able to SSH straight in (recommended), you must replace hardcoded pubkey
#    further down in the section labelled "Replace this by your SSH pubkey" by you own,
#    and host the modified script way under a URL of your choosing
#    (e.g. gist.github.com with git.io as URL shortener service).
# 3. Run on the client:
#
#       hcloud server attach-iso h8tsner nixos-minimal-22.05.590.f05e7d39a48-x86_64-linux.iso
#       hcloud server rebuild h8tsner --image ubuntu-22.04
#
#       XqVNLKaaMUFvnKvrgTkw
#       hcloud server enable-rescue --ssh-key GPG h8tsner
#
#       hcloud server attach-iso h8tsner nixos-minimal-22.05.590.f05e7d39a48-x86_64-linux.iso
#       hcloud server reboot h8tsner
#
#       hcloud server enable-rescue --ssh-key GPG h8tsner
#       hcloud server poweroff h8tsner
#       hcloud server poweron h8tsner
#       ssh-copy-id nixos@h8tsner
#       scp $DOTFILES_DIR/nixos/machines/h8tsner/nixos-install-hetzner-cloud.sh nixos@h8tsner:
#       ssh nixos@h8tsner 'sudo bash -c nixos-install-hetzner-cloud.sh'
#
#       hcloud server detach-iso h8tsner
#       hcloud server poweron h8tsner
#       ssh-keygen -R 5.161.105.71
#       ssh-copy-id root@h8tsner
#
#       deploy --skip-checks .#h8tsner -- --print-build-logs
#       hcloud server reboot h8tsner
#       hcloud server poweron h8tsner
#
#    This will install NixOS and power off the server.
# 4. Unmount the ISO image from the Hetzner Cloud GUI.
# 5. Turn the server back on from the Hetzner Cloud GUI.
#
# To run it from the Hetzner Cloud web terminal without typing it down,
# you can either select it and then middle-click onto the web terminal, (that pastes
# to it), or use `xdotool` (you have e.g. 3 seconds to focus the window):
#
#     sleep 3 && xdotool type --delay 50 'curl YOUR_URL_HERE | sudo bash'
#
# (In the xdotool invocation you may have to replace chars so that
# the right chars appear on the US-English keyboard.)
#
# If you do not replace the pubkey, you'll be running with my pubkey, but you can
# change it afterwards by logging in via the Hetzner Cloud web terminal as `root`
# with empty password.

set -e

# Hetzner Cloud OS images grow the root partition to the size of the local
# disk on first boot. In case the NixOS live ISO is booted immediately on
# first powerup, that does not happen. Thus we need to grow the partition
# by deleting and re-creating it.
sgdisk -d 1 /dev/sda
sgdisk -N 1 /dev/sda
partprobe /dev/sda

mkfs.ext4 -F /dev/sda1 # wipes all data!

mount /dev/sda1 /mnt

nixos-generate-config --root /mnt

# Delete trailing `}` from `configuration.nix` so that we can append more to it.
sed -i -E 's:^\}\s*$::g' /mnt/etc/nixos/configuration.nix

# Extend/override default `configuration.nix`:
echo '
  boot.loader.grub.devices = [ "/dev/sda" ];

  # Initial empty root password for easy login:
  users.users.root.initialHashedPassword = "";
  services.openssh.permitRootLogin = "prohibit-password";

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    # Replace this by your SSH pubkey!
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBxw8UnnH5Cizu7p9r4PFGDe/azUrdC0qA3K9GtWtvf/+l4dy044X3mI+hHVigTbxDH5viYcTiH6Lk+SHl2uZuX6fkzTBaFoonEJrKeCRS25TTMmas9g7D/maDoENEF1X0acs5Ffk3CAqKlOeynGPnj4M1ovUM8wyg1lsfZXA+LVr9GLLziiZSxVBBjG341hfVP3LFijj8qIAoDnBPrlLBjrrCsHXZa1QxjjyQADC5Ty7wgqLZqhfEEmkSdUEdkEt1lW4wzJzNXM/7F+iBmLTTp2KcUTPP2kyCU8YR+QvOMafB7ufmRoMf2ERjQtCwSJCYfEot3DBOvdgL0lFBTW4T /Users/cfeeley/.ssh/id_rsa"
  ];
}
' >>/mnt/etc/nixos/configuration.nix

nixos-install --no-root-passwd

poweroff
