{ config
, lib
, pkgs
, ...
}: {

  services.redshift = {
    enable = true;
  };
  services.autorandr = {
    enable = true;
    profiles = {
      "desktop" = {
        fingerprint = {
          DP-0 = "00ffffffffffff004c2d4d0c4c373930191f0104b53d23783a5fb1a2574fa2280f5054bfef80714f810081c08180a9c0b300950001014dd000a0f0703e80302035005f592100001a000000fd00384b1e873c000a202020202020000000fc00553238453539300a2020202020000000ff00484e4d523630313430320a2020019902030ef041102309070783010000023a801871382d40582c45005f592100001e565e00a0a0a02950302035005f592100001a04740030f2705a80b0588a005f592100001e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000052";
          HDMI-0 = "00ffffffffffff004c2d940f4e3739301b1f0103804627782ace51a6574c9f26125054bfef80714f810081c081809500a9c0b300010108e80030f2705a80b0588a00b9882100001e000000fd00184b1e873c000a202020202020000000fc00553332523539780a2020202020000000ff00484e41523630343336390a20200128020334f04d611203130420221f105f605d5e23090707830100006d030c001000b83c20006001020367d85dc401788003e30f0104023a801871382d40582c4500b9882100001e023a80d072382d40102c4580b9882100001e04740030f2705a80b0588a00b9882100001e565e00a0a0a0295030203500b9882100001a000000a0";
          DP-2 = "00ffffffffffff004c2d4d0c4a534d300c1d0104b53d23783a5fb1a2574fa2280f5054bfef80714f810081c08180a9c0b300950001014dd000a0f0703e80302035005f592100001a000000fd00384b1e873c000a202020202020000000fc00553238453539300a2020202020000000ff004854504d3330313838340a2020016b02030ef041102309070783010000023a801871382d40582c45005f592100001e565e00a0a0a02950302035005f592100001a04740030f2705a80b0588a005f592100001e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000052";
        };
        config = {
          DP-0 = {
            enable = true;
            crtc = 0;
            primary = false;
            position = "0x0";
            mode = "3840x2160";
            gamma = "1.0:0.909:0.833";
            rate = "60.00";
          };
          HDMI-0 = {
            enable = true;
            crtc = 0;
            primary = true;
            position = "3840x0";
            mode = "3840x2160";
            gamma = "1.0:0.909:0.833";
            rate = "60.00";
          };
          DP-2 = {
            enable = true;
            crtc = 0;
            primary = false;
            position = "7680x0";
            mode = "3840x2160";
            gamma = "1.0:0.909:0.833";
            rate = "60.00";
          };
        };
      };
    };
  };

  services.xserver = {
    ###
    ### Monitor config
    ### Note: monitors will be mapped from left to right in the order of the list.
    xrandrHeads = [
      # [ <- ]
      { output = "DP-2"; primary = false; } # R
      # [ <- ]

      # [ ⚪ ]
      { output = "HDMI-0"; primary = true; } # C
      # [ ⚪ ]

      # [ -> ]
      { output = "DP-0"; primary = false; } # L
      # [ -> ]
    ];

    # Pre-select HM xsession
    # displayManager.defaultSession = "none+xmonad";

    # Must contain 'A list of packages containing x11 or wayland session files to be passed to the display manager.'
    # displayManager.sessionPackages = [];

    ###
    ### XMonad (via home-manager)
    ###
    desktopManager.session = [
      {
        manage = "desktop";
        name = "none+xterm";
        start = ''
          ${pkgs.xterm}/bin/xterm -ls &
          waitPID=$!
        '';
      }
      {
        manage = "desktop";
        name = "none+xmonad";
        start = ''
          ${pkgs.xmonad-config}/bin/xmonad &
          waitPID=$!
        '';
      }
      {
        # Run Xmonad xsession from home-manager
        manage = "desktop";
        name = "session";
        start = ''
          ${pkgs.zsh} $HOME/.xsession &
          waitPID=$!
        '';
      }
    ];
  };

  # Prevent Java applications only displaying a blank window under tiling WMs
  # NOTE: Fixes Vivado white screen!
  environment.sessionVariables._JAVA_AWT_WM_NONREPARENTING = 1;
}
