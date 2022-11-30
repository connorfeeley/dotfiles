channels: final: prev:
let
  packagesFrom = inputAttr: inputAttr.packages.${final.system};
in
{
  __dontExport = true;

  ripgrep = prev.ripgrep.override { withPCRE2 = true; };

  xmonad = prev.xmonad-config;
  xmobar = prev.xmobar-config;

  inherit (channels.nixos-21-11)
    sourcetrail
    ;

  # FIXME: nix-zsh-completions are broken
  # https://github.com/NixOS/nixpkgs/pull/202750/files
  nix-zsh-completions = prev.nix-zsh-completions.overrideAttrs (o: {
    postPatch = ''
      rm _nix
    '';
  });

  # Genarate info pages for nixpkgs
  # Source: github:aakropotkin/nixpkgs-doc
  nixpkgsDoc = prev.htmlDocs.nixpkgsManual.overrideAttrs (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ prev.texinfo ];
    postBuild = ''
      mkdir texi info
      sed 's/\(<title><function>\)lib\.[^.]*\.\?\([^.<][^<]\+<\/function><\/title>\)/\1\2/'  \
        manual-full.xml > manual-full-nodots.xml
      pandoc -t texinfo -f docbook -o texi/nixpkgs.texi  \
             manual-full-nodots.xml
      makeinfo --force -o info texi/nixpkgs.texi
    '';
    installPhase = ''
      runHook preInstall
      ${oldAttrs.installPhase}
      runHook postInstall
    '';
    postInstall = ''
      cp -pr --reflink=auto -- info "$out/share/"
    '';
  });

  quartus-prime-lite = prev.quartus-prime-lite.overrideAttrs (finalAttrs: previousAttrs: {
    targetPkgs = previousAttrs.targetPkgs ++ [ prev.libcrypt ];
  });
}
