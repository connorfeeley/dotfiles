{ config, lib, pkgs, ... }: {
  services.xserver.displayManager.defaultSession = "xfce";
  environment.systemPackages = with pkgs; [
    xfce.xfce4-whiskermenu-plugin
    xfce.xfce4-windowck-plugin
    xfce.xfce4-pulseaudio-plugin
    xfce.xfce4-timer-plugin
    xfce.xfce4-time-out-plugin
    xfce.xfce4-taskmanager
    xfce.xfce4-systemload-plugin
    xfce.xfce4-sensors-plugin
    xfce.xfce4-netload-plugin

    xfce.xfce4-cpufreq-plugin
    xfce.xfce4-cpugraph-plugin
    xfce.xfce4-clipman-plugin
  ];
  services.picom = {
    enable = true;
    fade = true;
    inactiveOpacity = 0.9;
    shadow = true;
    fadeDelta = 4;
  };
  services.xserver = {
    enable = true;
    desktopManager = {
      xfce = {
        enable = true;
        noDesktop = false;
        enableXfwm = true;
      };
    };
  };
}
