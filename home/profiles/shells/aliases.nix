{
  mkdir = "mkdir -pv";

  # Use Kitty terminal"s ssh helper kitten
  sshk = "kitty +kitten ssh -o SendEnv=DOTFILES_OS_APPEARANCE -A";
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

  tree = "eza --tree";

  # Flush DNS cache
  flushdns = "dscacheutil -flushcache";

  mkcd = "mkdir -p $1 && cd $1";

  # Nix aliases
  nix = "nix --print-build-logs --show-trace";
  nis = "nix search nixpkgs";
  nir = "nix run";

  rsc = "rsync -rav --progress";

  rl = "readlink -f";

  s = "ssh";
}
