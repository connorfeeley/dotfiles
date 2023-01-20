{ name
, writeShellApplication
, nixos-rebuild
}:

writeShellApplication {
  inherit name;
  runtimeInputs = [ nixos-rebuild];
  text = ''
    # Check arguments and print usage
    [ $# -lt 1 ] || [ "$1" == "--help" ] || [ "$1" == "-h" ] && echo "Usage: $0 <[user@]remote host> ARGS" && exit 1

    # Pop remote hostname from arguments
    REMOTE=$1
    shift

    # Call nixos-rebuild with appropriate arguments for a remote build,
    # passing through all arguments except the first
    ${nixos-rebuild}/bin/nixos-rebuild \
        --build-host "$REMOTE" \
        --target-host "$REMOTE" \
        --use-substitutes \
        --use-remote-sudo \
        "$@"
  '';
}
