{ writeShellApplication, git, git-run }:
writeShellApplication {
  name = "dotfiles-sync";
  runtimeInputs = [ git git-run ];
  text = ''
    # Pull dotfiles and doomemacs (config) repos
    ${git-run}/bin/gr "$DOTFILES_DIR" "$DOOMDIR" ~/source/nurpkgs -- git pull --rebase --autostash
  '';
}
