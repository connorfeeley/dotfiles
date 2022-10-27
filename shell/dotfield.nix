# TODO: add dhall utils
{
  pkgs,
  extraModulesPath,
  inputs,
  lib,
  ...
}: let
  # These packages are available only in the default `pkgs` via external
  # overlays. We also need `system` to get the appropriate package set from the
  # unstable channel.
  inherit
    (pkgs)
    agenix
    nvfetcher-bin
    system
    ;
  inherit (pkgs.stdenv.buildPlatform) isLinux isi686;

  packages = inputs.nixos-unstable.legacyPackages.${system};

  inherit
    (packages)
    nixpkgs-fmt
    cachix
    editorconfig-checker
    nixUnstable
    nixos-rebuild
    rage
    shellcheck
    shfmt
    ssh-to-age
    terraform
    treefmt
    lefthook
    ;

  inherit (packages.nodePackages) prettier;

  nixos-generators = inputs.nixos-generators.defaultPackage.${pkgs.system};

  hooks = import ./hooks;

  withCategory = category: attrset: attrset // {inherit category;};
  pkgWithCategory = category: package: {inherit package category;};

  dotfield = pkgWithCategory "dotfield";
  linter = pkgWithCategory "linters";
  formatter = pkgWithCategory "formatters";
  utils = withCategory "utils";
  secrets = pkgWithCategory "secrets";
in {
  _file = toString ./.;

  name = "Dotfield";

  commands =
    [
      (dotfield nixUnstable)
      (dotfield inputs.deploy.packages.${pkgs.system}.deploy-rs)
      (dotfield terraform)
      (dotfield treefmt)
      (dotfield lefthook)

      {
        category = "dotfield";
        name = nvfetcher-bin.pname;
        help = nvfetcher-bin.meta.description;
        command = "cd $PRJ_ROOT/packages/sources; ${nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
      }

      (utils {
        name = "evalnix";
        help = "Check Nix parsing";
        command = "fd --extension nix --exec nix-instantiate --parse --quiet {} >/dev/null";
      })
      (utils {
        name = "watch-flake";
        help = "Continuously check flake";
        command = "fd . --extension=nix | entr -arc nix flake check";
      })

      (formatter nixpkgs-fmt)
      (formatter prettier)
      (formatter shfmt)

      (linter editorconfig-checker)
      (linter shellcheck)

      (secrets agenix)
      (secrets rage)
      (secrets ssh-to-age)
      {
        category = "secrets";
        name = "convert-keys";
        help = "helper to convert the usual ssh ed25519 keys to age keys";
        command = ''
          ${ssh-to-age}/bin/ssh-to-age -private-key -i ~/.ssh/id_ed25519 > ~/.config/sops/age/age-key.sec
          ${ssh-to-age}/bin/ssh-to-age -i ~/.ssh/id_ed25519.pub > ~/.config/sops/age/age-key.pub
        '';
      }
      {
        category = "ci";
        name = "flake-ci";
        help = "Show, check, then build the flake";
        command = let
          rebuild = if pkgs.stdenv.isLinux then "nixos-rebuild" else "darwin-rebuild";
        in ''
          ${nixUnstable}/bin/nix flake show --print-build-logs && \
            ${nixUnstable}/bin/nix flake check --print-build-logs && \
            ${rebuild} build --flake .#$HOSTNAME --print-build-logs
        '';
      }
    ]
    ++ lib.optional (!isi686) (dotfield cachix)
    ++ lib.optional isLinux (dotfield nixos-generators);
}
