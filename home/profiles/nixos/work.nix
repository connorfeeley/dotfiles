{ pkgs
, ...
}:
{
  ###
  ### Automatically shut off power bar at 8PM
  ###
  systemd.user.services.pwrbar-auto-off = {
    Unit = { Description = "Automatically shutoff Ultrix from WiFi power bar"; };
    Service = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.runtimeShell} -l -c "${pkgs.pwrbar}/bin/pwrbar off"
      '';
    };
  };
  systemd.user.timers.pwrbar-auto-off = {
    Unit = { Description = "Automatically shutoff Ultrix from WiFi power bar"; };
    Timer = {
      Unit = "pwrbar-auto-off.service";
      OnCalendar = "*-*-* 20:00:00";
    };
    Install = { WantedBy = [ "timers.target" ]; };
  };

  ###
  ### Backup work files
  ###
  systemd.user.services.dev-backup = with pkgs; {
    Unit = { Description = "Backup ~/dev directory nightly"; };
    Service = {
      Type = "oneshot";
      ExecStart = ''
        ${rsync}/bin/rsync --delete -rae "${openssh}/bin/ssh -o IdentityAgent=none" /home/cfeeley/dev/ cfeeley@cfeeley-laptop:development
      '';
    };
  };
  systemd.user.timers.dev-backup = {
    Unit = { Description = "Backup ~/dev directory nightly"; };
    Timer = {
      Unit = "dev-backup.service";
      OnCalendar = "*-*-* 20:00:00";
    };
    Install = { WantedBy = [ "timers.target" ]; };
  };
}
