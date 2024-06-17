{ writeShellApplication, git, nodePackages }:
writeShellApplication {
  name = "dotfiles-push";
  runtimeInputs = [ git nodePackages.git-run ];
  text = ''
    # Pull dotfiles and doomemacs (config) repos
    ${nodePackages.git-run}/bin/gr "$DOTFILES_DIR" "$DOOMDIR" ~/source/nurpkgs -- git push origin
  '';
}
