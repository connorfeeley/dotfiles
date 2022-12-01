{ config
, lib
, pkgs
, ...
}:
# Source: https://fy.blackhats.net.au/blog/html/2021/03/22/time_machine_on_samba_with_zfs.html

# Server setup: sudo smbpasswd -a cfeeley
# MacOS setup: tmutil setdestination smb://cfeeley:<password>@workstation/timemachine
{
  services.samba-wsdd.enable = true; # make shares visible for windows 10 clients
  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      ### GENERAL ###
      browseable = yes
      server string = workstation
      netbios name = workstation
      security = user

      use sendfile = yes

      # note: localhost is the ipv6 localhost ::1
      hosts allow = 100. 192.168.0. 127.0.0.1 localhost
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
    '' + ''
      ### TIME MACHINE ###
      min protocol = SMB2
      ea support = yes

      # This needs to be global else time machine ops can fail.
      vfs objects = fruit streams_xattr
      fruit:aapl = yes
      fruit:metadata = stream
      fruit:model = MacSamba
      fruit:posix_rename = yes
      fruit:veto_appledouble = no
      fruit:nfs_aces = no
      fruit:wipe_intentionally_left_blank_rfork = yes
      fruit:delete_empty_adfiles = yes
      spotlight = no
    '';
    shares = {
      timemachine = {
        path = "/mnt/zfs/backup/time_machine";
        # "valid users" = "username";
        # public = "no";
        # writeable = "yes";
        #
        comment = "Time Machine";
        browseable = "yes";
        "fruit:model" = "MacSamba";
        "fruit:time machine" = "yes";
        "fruit:time machine max size" = "1050G";
        "write list" = "cfeeley";
        "create mask" = "0600";
        "directory mask" = "0700";
        "case sensitive" = "true";
        "default case" = "lower";
        "preserve case" = "no";
        "short preserve case" = "no";
      };
      # public = {
      #   path = "/mnt/Shares/Public";
      #   browseable = "yes";
      #   "read only" = "no";
      #   "guest ok" = "yes";
      #   "create mask" = "0644";
      #   "directory mask" = "0755";
      #   "force user" = "username";
      #   "force group" = "groupname";
      # };
      # Media = {
      #   path = "/mnt/zfs/media";
      #   browseable = "yes";
      #   "read only" = "no";
      #   "guest ok" = "no";
      #   "create mask" = "0644";
      #   "directory mask" = "0755";
      #   "force user" = "username";
      #   "force group" = "groupname";
      # };
      # private = {
      #   path = "/mnt/export/cfeeley";
      #   browseable = "yes";
      #   "read only" = "no";
      #   "guest ok" = "no";
      #   "create mask" = "0644";
      #   "directory mask" = "0755";
      #   "force user" = "username";
      #   "force group" = "groupname";
      # };
    };
  };
}
