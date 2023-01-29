{ lib
, stdenv
, jq
, sources
, writeShellScriptBin
, kitty
,
}:
lib.makeExtensible (_self: {
  # FIXME: provide a better indication that there can be multiple kitty windows in
  # a single platform window. perhaps 'tab' is better suited?
  #
  # TODO: note that `kitty` is inherited from PATH here due to constant build
  # issues on darwin
  getWindowByPlatformId = writeShellScriptBin "kitty-get-window-by-platform-id" ''
    kitty @ --to $KITTY_SOCKET ls \
      | ${jq}/bin/jq -r --argjson id "$1" \
        '.[] | select(.platform_window_id==$id)'
  '';

  # FIXME: this should depend on the yabai-sa-kickstart script because killing
  # Dock will unload the scripting addition...
  setAppIcon = writeShellScriptBin "kitty-set-app-icon" ''
    SCHEME="''${1:-dark}"
    cp ${sources.kitty-bortflower-icons.src}/kitty-$SCHEME.icns /Applications/kitty.app/Contents/Resources/kitty.icns
    rm /var/folders/*/*/*/com.apple.dock.iconcache
    killall Dock
  '';

  kittyTerminfo = stdenv.mkDerivation {
    pname = "kitty-terminfo";
    inherit (kitty) version src;

    dontBuild = true;
    installPhase = ''
      mkdir -p $out/lib/kitty/terminfo
      cp terminfo/k/kitty $out/share/terminfo/k
      terminfo_src=${if stdenv.isDarwin then
        ''"$out/Applications/kitty.app/Contents/Resources/terminfo"''
        else
        "$out/share/terminfo"}

      mkdir -p $terminfo/share
      mv "$terminfo_src" $terminfo/share/terminfo

      mkdir -p $out/nix-support
      echo "$terminfo" >> $out/nix-support/propagated-user-env-packages

      cp -r 'shell-integration' "$shell_integration"
    '';

    outputs = [ "terminfo" ];
  };
})
