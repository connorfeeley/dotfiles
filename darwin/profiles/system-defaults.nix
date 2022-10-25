{ config
, lib
, pkgs
, ...
}: {
  imports = [ ];

  # Symlink Nix applications to '~/Applications/Nix Apps'
  # https://github.com/nix-community/home-manager/issues/1341#issuecomment-1190875080
  # https://github.com/LnL7/nix-darwin/pull/487
  # FIXME: ~/Applications must be created manually first
  system.activationScripts.applications.text = pkgs.lib.mkForce (
    ''
      echo "setting up ~/Applications..." >&2
      rm -rf ~/Applications/Nix\ Apps
      mkdir -p ~/Applications/Nix\ Apps
      for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
        src="$(/usr/bin/stat -f%Y "$app")"
        cp -r "$src" ~/Applications/Nix\ Apps
      done
    ''
  );

  security.pam.enableSudoTouchIdAuth = true;

  # TODO: why disabled? caused an error?
  # system.defaults.".GlobalPreferences".com.apple.sound.beep.sound = "Funk";

  # system.defaults.universalaccess = {
  #   reduceTransparency = true;
  #   closeViewScrollWheelToggle = true;
  #   closeViewZoomFollowsFocus = true;
  # };

  # Set arbitrary defaults
  system.defaults.CustomUserPreferences = {
    "com.apple.dock" = {
      "slow-motion-allowed" = true;
    };

    # Don't litter. (disable .DS_Store file creation)
    "com.apple.desktopservices" = {
      "DSDontWriteNetworkStores" = true;
    };
  };

  system.defaults.smb = {
    NetBIOSName = config.networking.hostName;
    ServerDescription = config.networking.hostName;
  };

  system.defaults.ActivityMonitor = {
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
    ShowCategory = 102;
    # Which column to sort the main activity page by
    SortColumn = "CPUUsage";
    SortDirection = 0; # 0: Descending; 1: Ascending
  };

  system.defaults.NSGlobalDomain = {
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
    NSWindowResizeTime = 0.001;
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
  };

  # Prevent incessant nagging when opening downloaded apps.
  system.defaults.LaunchServices.LSQuarantine = false;
  # macOS updates frequently nuke Nix, requiring a partial reinstall.
  system.defaults.SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

  # Firewall
  system.defaults.alf = {
    allowdownloadsignedenabled = 0;
    allowsignedenabled = 1;
    globalstate = 0;
    loggingenabled = 0;
    stealthenabled = 0;
  };

  system.defaults.dock = {
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

  system.defaults.finder = {
    AppleShowAllExtensions = true;
    # Whether to display icons on the desktop.
    CreateDesktop = false;
    FXEnableExtensionChangeWarning = false;
    FXPreferredViewStyle = "Nlsv";
    QuitMenuItem = true;
    ShowPathbar = true;
    ShowStatusBar = true;
    _FXShowPosixPathInTitle = false;
  };

  system.defaults.loginwindow = {
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

  system.defaults.spaces = {
    # spans-displays: true;
  };

  system.defaults.trackpad = {
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

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = false;
  };
  fonts.fontDir.enable = true;
}
