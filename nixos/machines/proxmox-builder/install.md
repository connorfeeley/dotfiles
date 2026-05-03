# proxmox-builder — manual install

Bootstrap procedure for the x86_64-linux remote builder VM. There is no
existing x86_64-linux builder reachable, so the flake configuration cannot
be pushed via `deploy-rs` from another host. Instead we install a minimal
NixOS, then build the full flake configuration *on the VM itself*. After
that, the VM becomes the reachable Linux builder for everything else.

## 1. Create the VM in Proxmox

- BIOS: SeaBIOS (no UEFI)
- Machine: q35
- SCSI controller: VirtIO SCSI single
- Disk: 100+ GB, virtio-scsi, on local-lvm or whichever pool
- CPU: host, 8+ vCPU
- Memory: 16+ GB
- Network: virtio, bridged to your LAN bridge (typically `vmbr0`)
- ISO: latest NixOS minimal x86_64 ISO

## 2. Boot the ISO and partition

In the live ISO console (auto-login as `nixos`):

```sh
sudo -i
parted /dev/sda -- mklabel msdos
parted /dev/sda -- mkpart primary ext4 1MiB 100%
parted /dev/sda -- set 1 boot on
mkfs.ext4 -L nixos /dev/sda1
mount /dev/disk/by-label/nixos /mnt
```

## 3. Generate config and tweak

```sh
nixos-generate-config --root /mnt
```

Edit `/mnt/etc/nixos/configuration.nix`. This is throwaway config — the only
job is to get a bootable system with SSH, git, and your pubkey so you can
clone the flake and rebuild. Inside the top-level attrset, add:

```nix
networking.hostName = "proxmox-builder";
networking.useDHCP = true;
services.openssh.enable = true;

users.mutableUsers = false;
users.users.cfeeley = {
  isNormalUser = true;
  extraGroups = [ "wheel" ];
  # Same hash used by the flake's default.nix so the password stays consistent
  # across the install-time and post-rebuild systems. Generate a fresh one with
  # `mkpasswd -m sha-512` if you want a different password for the throwaway
  # install.
  initialHashedPassword =
    "$6$V/uLpKYBvGk/Eqs7$IMguTPDVu5v1B9QBkPcIi/7g17DPfE6LcSc48io8RKHUjJDOLTJob0qYEaiUCAS5AChK.YOoJrpP5Bx38XIDB0";
  openssh.authorizedKeys.keys = [
    # paste the contents of secrets/authorized-keys.nix here
  ];
};
nix.settings.trusted-users = [ "root" "cfeeley" "@wheel" ];
nix.settings.experimental-features = [ "nix-command" "flakes" ];

environment.systemPackages = with pkgs; [ git ];

boot.loader.grub.enable = true;
boot.loader.grub.device = "/dev/sda";
```

## 4. Install and reboot

```sh
nixos-install --no-root-passwd
reboot
```

Detach the ISO from the VM before it boots back up.

## 5. Capture host key + hardware config (from the Mac)

```sh
# Hardware config — commit alongside default.nix.
scp cfeeley@proxmox-builder:/etc/nixos/hardware-configuration.nix \
    nixos/machines/proxmox-builder/hardware-configuration.nix

# Host key — used in two follow-up edits below.
ssh cfeeley@proxmox-builder cat /etc/ssh/ssh_host_ed25519_key.pub | tee /tmp/pmx.pub
base64 -i /tmp/pmx.pub          # → for darwin/profiles/distributed-build.nix
cat /tmp/pmx.pub                # → for secrets/secrets.nix
```

If the new `hardware-configuration.nix` differs from the placeholder already
in this directory (different fs UUID, different kernel modules), commit the
update. The grub bootloader config lives in `default.nix`, not here.

## 6. Wire the host key into the flake (on the Mac)

- `secrets/secrets.nix`: add a `proxmox-builder` entry next to `h8tsner` and
  append it to the `systems` list. Do **not** run `agenix -r` — the existing
  `*.age` files don't need to be decryptable on this host (it's a builder,
  not a service host), and the local Mac key is currently out of sync with
  the recipients list anyway. Any new secrets created going forward will pick
  up `proxmox-builder` automatically.
- `darwin/profiles/distributed-build.nix`: append a buildMachines entry for
  `proxmox-builder` (and mirror it in `nix.settings.builders`) using the
  base64'd host key from step 5.

Push these commits to the remote so the VM can pull them in step 7.

## 7. Build the flake config on the VM itself

SSH to the VM and clone the flake. The VM is x86_64-linux and Nix is on the
freshly installed system, so it can build everything locally.

```sh
ssh cfeeley@proxmox-builder
sudo -i
mkdir -p /etc/nixos && cd /etc/nixos
git clone <flake-remote> dotfiles
cd dotfiles
nixos-rebuild switch --flake .#proxmox-builder
```

This first build will be expensive (no cache hits beyond cache.nixos.org +
your existing substituters). Once it lands, `nix.sshServe` is up and the
VM is a usable remote builder.

## 8. Bring the Mac online as a client

```sh
darwin-rebuild switch --flake .
```

Verify:

```sh
nix store ping --store ssh-ng://cfeeley@proxmox-builder
nix build --impure --expr '(import <nixpkgs> { system = "x86_64-linux"; }).hello' -L
```

## 9. Future updates

Subsequent rebuilds can go through deploy-rs from the Mac (the Mac will use
the proxmox-builder itself to do the build):

```sh
deploy .#proxmox-builder -- --print-build-logs
```
