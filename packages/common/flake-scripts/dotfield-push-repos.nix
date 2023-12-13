{ writeShellApplication, git, nodePackages }:
writeShellApplication {
  name = "dotfield-push";
  runtimeInputs = [ git nodePackages.git-run ];
  text = ''
    # Pull dotfield and doomemacs (config) repos
    ${nodePackages.git-run}/bin/gr "$DOTFIELD_DIR" "$DOOMDIR" ~/source/nurpkgs -- git push origin
  '';
}
