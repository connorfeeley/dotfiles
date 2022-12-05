{ pkgs
, config
, ...
}:
let
  vimariUserSettings = {
    bindings = {
      closeTab = "x";
      goBack = "shift+h";
      goForward = "shift+l";
      goToFirstInput = "g i";
      goToPageBottom = "shift+g";
      goToPageTop = "g g";
      hintToggle = "f";
      newTabHintToggle = "shift+f";
      openTab = "t";
      reload = "r";
      scrollDown = "j";
      scrollDownHalfPage = "d";
      scrollLeft = "h";
      scrollRight = "l";
      scrollUp = "k";
      scrollUpHalfPage = "u";
      tabBack = "q";
      tabForward = "w";
    };
    detectByCursorStyle = false;
    excludedUrls = "";
    linkHintCharacters = "asdfjklqwerzxc";
    modifier = "";
    openTabUrl = "topsites://";
    scrollDuration = 25;
    scrollSize = 150;
    smoothScroll = true;
    transparentBindings = true;
  };
in
{
  homebrew.masApps = {
    "Vimari" = 1480933944; # Safari port of Vimium
    "AdGuard for Safari" = 1440147259; # Not quite uBlock origin
    "Tweaks for Reddit" = 1524828965; # RES-lite
  };

  home-manager.users.${config.dotfield.guardian.username} = hmArgs: {
    home.file."Library/Containers/net.televator.Vimari.SafariExtension/Data/Library/Application Support/userSettings.json".source =
      pkgs.writeText "userSettings.json" (builtins.toJSON vimariUserSettings);
  };
}
