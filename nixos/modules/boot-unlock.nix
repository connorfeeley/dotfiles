###
### Source: github:antifuchs | https://gist.github.com/antifuchs/e30d58a64988907f282c82231dde2cbc
###

# To set this up, first get tailscale working in an isolated linux shell:
#  1. sudo systemctl stop tailscaled.service
#  2. tailscaled -port 9993 -state tailscale-luks-setup.state -tun userspace-networking -socket ./tailscaled.sock
#  3. tailscale -socket ./tailscaled.sock up -hostname HOSTNAME-luks
#  4. tailscale -socket ./tailscaled.sock down
#  5. ctrl-c out of tailscaled
#  6 sudo systemctl start tailscaled.service
#
# Then add the .state file to your machine secrets and pass its path as tailscaleStatePath.

{ config, lib, pkgs, ... }: {
  options = {
    remote-machine.boot.tailscaleUnlock = with lib; {
      enable = mkOption {
        description = "Turn on unlock via tailscale";
        default = false;
      };

      tailscaleStatePath = mkOption {
        description = "Pre-initialized tailscale state file as a secret. Make sure to set it to not require re-authentication, otherwise the machine may not boot up after a few weeks.";
      };
    };
  };

  config =
    let
      cfg = config.remote-machine.boot.tailscaleUnlock;
      # TODO: This uses old-style non-nftables iptables; ideally, we wouldn't have to opt out of that.
      # Enabling nftables compat means having to shuffle the list of
      # modules down in availableKernelModules; that's a bunch of work
      # (deploying to a linux machine & rebooting to see what doesn't
      # work this time), so I'm a bit too lazy for that now.
      iptables-static = (pkgs.iptables.override { nftablesCompat = false; }).overrideAttrs (old: {
        dontDisableStatic = true;
        configureFlags = (lib.remove "--enable-shared" old.configureFlags) ++ [
          "--enable-static"
          "--disable-shared"
        ];
      });
    in
    lib.mkIf cfg.enable {
      assertions = [{
        assertion =
          let
            dhcpInterfaces = lib.attrNames (lib.filterAttrs (iface: v: v.useDHCP == true) (config.networking.interfaces or { }));
            doDhcp = config.networking.useDHCP || dhcpInterfaces != [ ];
          in
          doDhcp;
        message = ''
          Must be using DHCP or have at least 1 interface using DHCP.

          See <nixpkgs>/nixos/modules/system/boot/initrd-network.nix for options.
        '';
      }];

      boot.initrd = {
        # secrets = {
        #   "/etc/ssl/certs/ca-certificates.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        #   "/etc/ssl/certs/ca-bundle.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        # };
        network = {
          enable = true;
          flushBeforeStage2 = true;
          postCommands = ''
            # Bring up tailscaled and dial in
            echo 'nameserver 8.8.8.8' > /etc/resolv.conf
            mkdir /dev/net
            mknod /dev/net/tun c 10 200
            .tailscaled-wrapped 2>/dev/null &
            sleep 5
            echo "Starting tailscale..."
            .tailscale-wrapped up
            .tailscale-wrapped status

            echo "echo 'Use cryptsetup-askpass to unlock!'" >> /root/.profile
          '';
        };
        availableKernelModules = [
          "ip6_tables"
          "ip6table_filter"
          "ip6table_nat"
          "ip6table_raw"
          "ip_tables"
          "iptable_filter"
          "iptable_nat"
          "iptable_raw"
          "nf_conntrack"
          "nf_nat"
          "tun"
          "xt_comment"
          "xt_conntrack"
          "xt_mark"
          "xt_MASQUERADE"
          "xt_LOG"
          "xt_tcpudp"
        ];
        extraUtilsCommands = ''
          copy_bin_and_libs ${pkgs.tailscale}/bin/.tailscaled-wrapped
          copy_bin_and_libs ${pkgs.tailscale}/bin/.tailscale-wrapped
          copy_bin_and_libs ${pkgs.iproute}/bin/ip
          copy_bin_and_libs ${iptables-static}/bin/iptables
          copy_bin_and_libs ${iptables-static}/bin/xtables-legacy-multi

          copy_bin_and_libs ${pkgs.strace}/bin/strace
        '';
        postMountCommands = ''
          # tear down tailscale
          pkill .tailscaled-wrapped
          .tailscaled-wrapped --cleanup
        '';
      };
    };
}
