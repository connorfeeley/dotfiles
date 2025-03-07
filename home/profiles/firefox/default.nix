moduleArgs@{ config, lib, pkgs, inputs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux isAarch64;
  inherit (pkgs.nur.repos.rycee) firefox-addons;

  themeFonts = config.theme.font;

  cfg = config.programs.firefox;

  isBukuEnabled = config.programs.buku.enable
    && config.programs.buku.enableBrowserIntegration;

  # via https://github.com/nix-community/home-manager/blob/e1f1160284198a68ea8c7fffbbb1436f99e46ef9/modules/programs/firefox.nix#L11-L20

  hostName =
    moduleArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");
  lepton = import ./lepton.nix;

  disableTelemetry = {
    "browser.newtabpage.activity-stream.feeds.telemetry" = false;
    "browser.newtabpage.activity-stream.telemetry" = false;
    "browser.ping-centre.telemetry" = false;
    "toolkit.telemetry.archive.enabled" = false;
    "toolkit.telemetry.bhrPing.enabled" = false;
    "toolkit.telemetry.enabled" = false;
    "toolkit.telemetry.firstShutdownPing.enabled" = false;
    "toolkit.telemetry.hybridContent.enabled" = false;
    "toolkit.telemetry.newProfilePing.enabled" = false;
    "toolkit.telemetry.reportingpolicy.firstRun" = false;
    "toolkit.telemetry.shutdownPingSender.enabled" = false;
    "toolkit.telemetry.unified" = false;
    "toolkit.telemetry.updatePing.enabled" = false;
  };

  privacySettings = {
    "network.dns.disablePrefetch" = true;
    "privacy.donottrackheader.enabled" = true;
    "privacy.donottrackheader.value" = 1;
    "privacy.trackingprotection.enabled" = true;
    "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
    "privacy.trackingprotection.socialtracking.enabled" = true;
  };

  defaultSettings = disableTelemetry // {
    "app.update.auto" = true;
    "browser.bookmarks.showMobileBookmarks" = true;
    "browser.ctrlTab.recentlyUsedOrder" = false;
    "browser.proton.enabled" = true;
    "browser.newtabpage.enabled" = true;

    # Enable a real search bar on `about:home` instead of diverting focus to the address bar.
    "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" =
      false;

    "browser.search.hiddenOneOffs" = "Google,Yahoo,Bing,Amazon.com,Twitter";
    "browser.search.region" = "US";
    "browser.search.suggest.enabled" = true;
    "browser.send_pings" = false;
    "browser.startup.homepage" = "https://lobste.rs";

    # 0 = Normal; 1 = Compact; 2 = Touch
    "browser.uidensity" = 1;

    "browser.urlbar.placeholderName" = "…";
    "browser.urlbar.showSearchSuggestionsFirst" = false;
    "browser.urlbar.suggest.calculator" = true;
    "browser.urlbar.suggest.history" = true;

    "devtools.theme" = config.colorscheme.kind;

    "extensions.pocket.enabled" = false;

    # Allow extensions to run on Mozilla domains.
    # Required for Tridactyl and Dark Reader support on those pages.
    # See https://github.com/tridactyl/tridactyl/issues/1800
    "extensions.webextensions.restrictedDomains" = "";

    # FIXME: use global font defaults
    "font.default.x-western" = "sans-serif";
    "font.name.monospace.x-western" = themeFonts.mono.family;
    "font.name.sans-serif.x-western" = themeFonts.sans.family;
    "font.name.serif.x-western" = themeFonts.serif.family;
    "font.size.monospace.x-western" = themeFonts.mono.size;

    "identity.fxaccounts.account.device.name" = hostName;

    # CSS blur filter in v88+
    "layout.css.backdrop-filter.enabled" = true;

    # Disable fingerprinting on AMO for Tridactyl.
    # See https://github.com/tridactyl/tridactyl/issues/1800
    "privacy.resistFingerprinting.block_mozAddonManager" = true;

    "security.enterprise_roots.enabled" = true;
    "services.sync.declinedEngines" =
      "addons,prefs,creditcards,addresses,tabs,passwords";
    "services.sync.engine.addons" = false;
    "services.sync.engine.passwords" = false;
    "services.sync.engine.prefs" = false;
    "services.sync.engineStatusChanged.addons" = true;
    "services.sync.engineStatusChanged.prefs" = true;
    "signon.rememberSignons" = false;

    # https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-context-properties
    "svg.context-properties.content.enabled" = true;

    # Enable custom stylesheets.
    "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  };

  styles = {
    dotfiles = {
      userChrome = ''
        /* Load Dotfiles customisations. */
        /* Note: the entire content of this file is copied below, so the file itself isn't imported. */
        @import url("${toString ./userChrome.css}");

        * {
          font-family: ${themeFonts.mono.family}, monospace !important;
          /* font-size: ${builtins.toString themeFonts.mono.size}px; */
        }
      '';
      userContent = ''
        /* Load Dotfiles customisations. */
        @import url("${toString ./userContent.css}");
      '';
    };
    lepton = {
      userChrome = ''
        /* Load Lepton userChrome.css */
        @import url("${inputs.firefox-lepton.outPath}/userChrome.css");
      '';
      userContent = ''
        /* Load Lepton userContent.css */
        @import url("${inputs.firefox-lepton.outPath}/userContent.css");
      '';
    };
  };
in
{
  xdg.configFile."tridactyl".source = ./tridactyl;

  # programs.buku = {
  #   enable = true;
  #   enableBrowserIntegration = true;
  # };

  programs.firefox = {
    enable = true;
    package =
      if isDarwin
      # Handled by the Homebrew module
      # This populates a dummy package to satisfy the requirement
      then
        pkgs.runCommand "firefox-0.0.0" { } "mkdir $out"
      # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/applications/networking/browsers/firefox/wrapper.nix
      else
        (pkgs.firefox.overrideAttrs (oldAttrs: {
          patches = (oldAttrs.patches or [ ]) ++ [
            (pkgs.fetchpatch {
              name = "mozilla-kde.patch";
              url = "https://raw.githubusercontent.com/openSUSE/firefox-maintenance/46e263fe8a7f39c74f17438c7b9745bb6ac36172/mozilla-kde.patch";
              hash = "sha256-oH1Y4KDdU5/Xs9sPJGAVu4NgxNM+YtJuKaHA3wY5y1o=";
            })

            # Emacs keybindings for the search bar: https://www.reddit.com/r/emacs/comments/iefqu2/remapping_ctrln_and_ctrlp_for_searchbar/
            (pkgs.fetchpatch {
              name = "remap_open_new_window_and_print_to_ALT_N_and_ALT_P.patch";
              url = "https://raw.githubusercontent.com/natask/firefox-tor-remap-C-n-and-C-p-for-search-bar-navigation/58603b9911ee037d94cb6329250301fe6a511e0c/remap_open_new_window_and_print_to_ALT_N_and_ALT_P.patch";
              hash = "sha256-VS9NyqRzw+vklNTgHhooh3NunlZdM2BO8GOce36Ww8E=";
            })
            (pkgs.fetchpatch {
              name = "navigating_up_and_down_with_C-p_C-n.patch";
              url = "https://raw.githubusercontent.com/natask/firefox-tor-remap-C-n-and-C-p-for-search-bar-navigation/58603b9911ee037d94cb6329250301fe6a511e0c/navigating_up_and_down_with_C-p_C-n.patch";
              hash = "sha256-kaAZg1Zhlrj23JeupgLrJs4YXZN1XreVRM8KnsyQsTw=";
            })
          ];
        })).override {
          cfg = {
            # Gnome shell native connector
            enableGnomeExtensions = moduleArgs.osConfig.services.gnome.gnome-browser-connector.enable;
          };
        };

    profiles.home = lib.mkIf isLinux {
      id = 0;

      settings = defaultSettings // privacySettings // lepton.settings.required
        // lepton.settings.theme.lepton // lepton.settings.recommended
        // lepton.settings.optional;

      extensions = with firefox-addons; [
        (lib.mkIf isBukuEnabled bukubrow)
        darkreader
        old-reddit-redirect
        org-capture
        react-devtools
        reddit-enhancement-suite
        tampermonkey
        refined-github
        return-youtube-dislikes
        sidebery
        sourcegraph
        tab-session-manager
        temporary-containers
        tridactyl
        ublock-origin

        bitwarden
        consent-o-matic
        sponsorblock
        sidebery
        vimium

        auto-tab-discard

        (lib.mkIf isLinux plasma-integration)

        ##: Themes {{

        theme-nord-polar-night

        ##: }}
      ];
      userChrome = ''
        ${styles.lepton.userChrome}

        :root {
          --dotfiles-tab-line-color: #5e81ac;
        }

        ${styles.dotfiles.userChrome}
      '';

      userContent = ''
        ${styles.lepton.userContent}
        ${styles.dotfiles.userContent}
      '';
    };
  };
}
##: References
#
# - https://github.com/cmacrae/config/blob/5a32507753339a2ee45155b78b76fda0824002a0/modules/macintosh.nix#L331-L407
# - https://restoreprivacy.com/firefox-privacy/
