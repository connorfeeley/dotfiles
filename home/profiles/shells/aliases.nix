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

  # Nix aliases
  nix = "nix --print-build-logs";
  nis = "nix search nixpkgs";
  nir = "nix run";
  nix-stray-roots = ''nix-store --gc --print-roots | egrep -v "^(/nix/var|/run/\w+-system|\{memory)"'';

  rsc = "rsync -rav --progress";

  rl = "readlink -f";
}
