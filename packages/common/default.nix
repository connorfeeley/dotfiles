{ pkgs, callPackage, ... }:
let
  nixFlags = [
    "--verbose"
    "--show-trace"
    "--print-build-logs"
  ];
  ### Nix 'aliases' (defined as executables so as to be usable from Emacs)
  # 'nix'
  n = pkgs.writeShellScriptBin "n" ''
    exec ${pkgs.nix}/bin/nix ${builtins.concatStringsSep " " nixFlags} "$@"
  '';
  # 'nix build'
  nb = pkgs.writeShellScriptBin "nb" ''
    exec ${pkgs.nix}/bin/nix build ${builtins.concatStringsSep " " nixFlags} "$@"
  '';
in
{
  # Nix 'aliases'
  inherit n nb;

  xsct = callPackage ./xsct.nix { };

  xantfarm = callPackage ./xantfarm.nix { };

  mdio-tools = callPackage ./mdio-tools.nix { };


  # FIXME: (2024-05-26): derivation changed upstream
  # nomos-rebuild = callPackage ./flake-scripts/nomos-rebuild.nix { };

  nixos-rebuild-remote =
    callPackage ./flake-scripts/nixos-rebuild-remote.nix {
      name = "nixos-rebuild-remote";
      inherit (pkgs) writeShellApplication nixos-rebuild;
    };

  ediff-tool = pkgs.writeShellScriptBin "ediff-tool" (builtins.readFile ./ediff-tool);
}
