{ lib
, writeShellApplication
, git
, git-run
}:

writeShellApplication {
  name = "dotfield-sync";
  runtimeInputs = [ git git-run ];
  text = ''
    # Pull dotfield and doomemacs (config) repos
    ${git-run}/bin/gr "$DOTFIELD_DIR" "$DOOMDIR" -- git pull --rebase --autostash
   '';
}
