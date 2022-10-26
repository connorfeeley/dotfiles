{
  mkdir = "mkdir -pv";

  # Use Kitty terminal"s ssh helper kitten
  sshk = "kitty +kitten ssh -o SendEnv=DOTFIELD_OS_APPEARANCE -A";
  # Display an image in kitty
  icat = "kitty +kitten icat";

  # Always enable colored `grep` output
  # Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
  grep = "grep --color=auto";
  fgrep = "fgrep --color=auto";
  egrep = "egrep --color=auto";

  xat = "hexyl";

  ".." = "cd ..";
  "..." = "cd ../..";
  "...." = "cd ../../..";
  "....." = "cd ../../../..";
  "......" = "cd ../../../../..";

  tree = "exa --tree";

  # Flush DNS cache
  flushdns = "dscacheutil -flushcache";

  mkcd = "mkdir -p $1 && cd $1";

  nix-stray-roots = ''nix-store --gc --print-roots | egrep -v "^(/nix/var|/run/\w+-system|\{memory)"'';

  rsc = "rsync -rav --progress";

  rl = "readlink -f";

  # Quick and dirty alias to push system flake and doom config
  sys-push = "gr $DOTFIELD_DIR $DOOMDIR -- git push"; # TODO: make less ugly

  # TODO: make a lot less ugly
  # Quick and dirty alias to pull system flake and doom, build and activate new system config, sync and build doom config
  nom-rebuild = let systemType = ''$([[ "$(uname)" == "Darwin" ]] && echo darwin || echo nixos)''; in
    ''gr $DOTFIELD_DIR $DOOMDIR -- git pull && pushd $DOTFIELD_DIR && nom build $DOTFIELD_DIR#${systemType}Configurations.$(hostname).config.system.build.toplevel && sudo $DOTFIELD_DIR/result/activate && doom clean && doom sync && doom build -r && doom doctor --pager cat; popd'';
}
