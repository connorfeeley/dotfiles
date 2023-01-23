{ lib, stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  pname = "fildem-global-menu";
  version = "dcd77d170822024ed7fc3c3781b3b77d1f076183";
  src = fetchFromGitHub {
    url = "https://extensions.gnome.org/extension-data/${
          builtins.replaceStrings [ "@" ] [ "" ] uuid
        }.v${builtins.toString version}.shell-extension.zip";
    inherit sha256;
    stripRoot = false;
    # The download URL may change content over time. This is because the
    # metadata.json is automatically generated, and parts of it can be changed
    # without making a new release. We simply substitute the possibly changed fields
    # with their content from when we last updated, and thus get a deterministic output
    # hash.
    postFetch = ''
      echo "${metadata}" | base64 --decode > $out/metadata.json
    '';
  };
  dontBuild = false;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/
    cp -r -T . $out/share/gnome-shell/extensions/${uuid}
    runHook postInstall
  '';
  # meta = {
  #   description = builtins.head (lib.splitString "\n" description);
  #   longDescription = description;
  #   homepage = link;
  #   license = lib.licenses.gpl2Plus; # https://wiki.gnome.org/Projects/GnomeShell/Extensions/Review#Licensing
  #   maintainers = with lib.maintainers; [ piegames ];
  # };
}
