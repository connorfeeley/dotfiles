{ config, lib, pkgs, ... }:
let inherit (pkgs.stdenv) isAarch64;
in {
  # Use touchID for authorizing sudo; but only on Apple Silicon
  security.pam.enableSudoTouchIdAuth = isAarch64;

  system.defaults = {
    # GlobalPreferences".com.apple.sound.beep.sound = "Funk";

    # universalaccess = {
    #   reduceTransparency = true;
    #   closeViewScrollWheelToggle = true;
    #   closeViewZoomFollowsFocus = true;
    # };

    # Set arbitrary defaults
    CustomUserPreferences = {
      "com.apple.dock" = { "slow-motion-allowed" = true; };

      # Don't litter. (disable .DS_Store file creation on network drives)
      "com.apple.desktopservices" = { "DSDontWriteNetworkStores" = true; };

      # Set replacement for iTunes / Apple Music
      "digital.twisted.noTunes" = { "replacement" = "/Applications/Spotify.app"; };
    };

    smb = {
      NetBIOSName = config.networking.hostName;
      ServerDescription = config.networking.hostName;
    };

    ActivityMonitor = {
      # 0: Application Icon; 2: Network Usage; 3: Disk Activity; 5: CPU Usage; 6: CPU History
      IconType = 6;
      # Open the main window when opening Activity Monitor. Default is true.
      OpenMainWindow = true;
      # Change which processes to show
      #  100: All Processes
      #  101: All Processes, Hierarchally
      #  102: My Processes
      #  103: System Processes
      #  104: Other User Processes
      #  105: Active Processes
      #  106: Inactive Processes
      #  107: Windowed Processes
      ShowCategory = 100;
      # Which column to sort the main activity page by
      SortColumn = "CPUUsage";
      SortDirection = 0; # 0: Descending; 1: Ascending
    };

    NSGlobalDomain = {
      # Whether light/dark modes are toggled automatically.
      AppleInterfaceStyleSwitchesAutomatically = true;
      AppleFontSmoothing = 0;
      AppleKeyboardUIMode = 3;
      AppleMeasurementUnits = "Centimeters";
      AppleMetricUnits = 1;
      ApplePressAndHoldEnabled = false; # '= true' breaks key repeat!
      AppleShowAllExtensions = false;
      AppleShowScrollBars = "Automatic";
      AppleTemperatureUnit = "Celsius";
      InitialKeyRepeat = 15;
      KeyRepeat = 3;
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      # Disable automatic termination of "inactive" apps.
      NSDisableAutomaticTermination = true;
      NSDocumentSaveNewDocumentsToCloud = false;
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
      NSScrollAnimationEnabled = true;
      NSTableViewDefaultSizeMode = 1;
      NSTextShowsControlCharacters = true;
      # Disable the over-the-top focus ring animation
      # NSUseAnimatedFocusRing = false;
      NSWindowResizeTime = 1.0e-3;
      PMPrintingExpandedStateForPrint = true;
      PMPrintingExpandedStateForPrint2 = true;
      # Whether to hide the menu bar.
      _HIHideMenuBar = lib.mkDefault false;
      # Use F1, F2, etc. keys as standard function keys.
      "com.apple.keyboard.fnState" = false;
      # Configures the trackpad tap behavior. Mode 1 enables tap to click.
      "com.apple.mouse.tapBehavior" = 1;
      # Apple menu > System Preferences > Sound Make a feedback sound when
      # the system volume changed. This setting accepts the integers 0 or
      # 1. Defaults to 1.
      "com.apple.sound.beep.feedback" = 1;
      # https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults.NSGlobalDomain.com.apple.sound.beep.volume
      # "com.apple.sound.beep.volume" = null;
      # Set the spring loading delay for directories. The default is the float `1.0`.
      "com.apple.springing.delay" = 0.1;
      # Enable spring loading (expose) for directories.
      "com.apple.springing.enabled" = true;
      # Disable "Natural" scrolling direction.
      "com.apple.swipescrolldirection" = true;
      # Whether to enable trackpad secondary click.
      "com.apple.trackpad.enableSecondaryClick" = true;
      # Configures the trackpad tracking speed (0 to 3). The default is "1".
      "com.apple.trackpad.scaling" = 1.0;
      # Configures the trackpad corner click behavior. Mode 1 enables right click.
      # https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults.NSGlobalDomain.com.apple.trackpad.trackpadCornerClickBehavior
      "com.apple.trackpad.trackpadCornerClickBehavior" = null;

      AppleEnableMouseSwipeNavigateWithScrolls = true;
      AppleEnableSwipeNavigateWithScrolls = true;
      AppleICUForce24HourTime = true;
    };

    # Prevent incessant nagging when opening downloaded apps.
    LaunchServices.LSQuarantine = false;
    # macOS updates frequently nuke Nix, requiring a partial reinstall.
    SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

    # Firewall
    alf = {
      allowdownloadsignedenabled = 0;
      allowsignedenabled = 1;
      globalstate = 0;
      loggingenabled = 0;
      stealthenabled = 0;
    };

    dock = {
      autohide = true;
      autohide-delay = 0.1;
      autohide-time-modifier = 0.1;
      dashboard-in-overlay = false;
      enable-spring-load-actions-on-all-items = false;
      expose-animation-duration = 0.1;
      expose-group-by-app = false;
      launchanim = false;
      mineffect = "genie";
      minimize-to-application = true;
      mouse-over-hilite-stack = true;
      mru-spaces = false;
      orientation = "bottom";
      show-process-indicators = true;
      show-recents = false;
      showhidden = true;
      static-only = false;
      tilesize = 64;
      # Disable bottom right corner hot action
      wvous-br-corner = 1;
    };

    finder = {
      AppleShowAllExtensions = true;
      # Whether to display icons on the desktop.
      CreateDesktop = false;
      FXEnableExtensionChangeWarning = false;
      FXPreferredViewStyle = "Nlsv";
      QuitMenuItem = true;
      ShowPathbar = true;
      ShowStatusBar = true;
      # Display the full folder path in the title bar.
      # Required for Talon.
      _FXShowPosixPathInTitle = true;
    };

    loginwindow = {
      DisableConsoleAccess = false;
      GuestEnabled = false;
      # Text to be shown on the login window. Default "\\U03bb" (lambda).
      # LoginwindowText = null;
      PowerOffDisabledWhileLoggedIn = false;
      RestartDisabled = false;
      RestartDisabledWhileLoggedIn = false;
      SHOWFULLNAME = false;
      ShutDownDisabled = false;
      ShutDownDisabledWhileLoggedIn = false;
      SleepDisabled = false;
    };

    # The filesystem path to which screencaptures should be written.
    # system.defaults.screencapture.location = null;

    spaces = {
      # spans-displays: true;
    };

    trackpad = {
      # 0 to enable Silent Clicking, 1 to disable. The default is 1.
      ActuationStrength = 1;
      # Whether to enable trackpad tap to click. The default is false.
      Clicking = true;
      # For normal click: 0 for light clicking, 1 for medium, 2 for firm. The default is 1.
      FirstClickThreshold = 1;
      # For force touch: 0 for light clicking, 1 for medium, 2 for firm. The default is 1.
      SecondClickThreshold = 1;
      TrackpadRightClick = true;
      TrackpadThreeFingerDrag = false;
    };
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = false;
  };
}
