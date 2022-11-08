# TODO: add dhall utils
{ pkgs
, extraModulesPath
, inputs
, lib
, ...
}:
let
  inherit (pkgs.stdenv) isLinux isDarwin isAarch64;

  inherit (pkgs)
    agenix
    nvfetcher-bin
    deploy-rs
    nixpkgs-fmt
    cachix
    editorconfig-checker
    nixUnstable
    rage
    shellcheck
    shfmt
    ssh-to-age
    terraform
    treefmt
    lefthook
    nix-output-monitor
    nixos-generators
    ;

  inherit (pkgs.nodePackages)
    prettier
    ;

  hooks = import ./hooks;

  scripts = pkgs.callPackage ./scripts { };

  withCategory = category: attrset: attrset // { inherit category; };
  pkgWithCategory = category: package: { inherit package category; };

  shortcut = pkgWithCategory "shortcut";
  dotfield = pkgWithCategory "dotfield";
  linter = pkgWithCategory "linters";
  formatter = pkgWithCategory "formatters";
  utils = pkgWithCategory "utils";
  ci = withCategory "ci";
  secrets = pkgWithCategory "secrets";
in
{
  _file = toString ./.;

  name = "Dotfield";

  commands =
    [
      (dotfield nixUnstable)
      (dotfield deploy-rs)
      (dotfield terraform)
      (dotfield cachix)
      (dotfield lefthook)
      {
        category = "dotfield";
        name = nvfetcher-bin.pname;
        help = nvfetcher-bin.meta.description;
        command = "cd $PRJ_ROOT/packages/sources; ${nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
      }
      (dotfield nixos-generators)
      (withCategory "dotfield" {
        name = "generate-h8tsner-kexec-bundle";
        help = "Use nixos-generate to build a kexec-build for the h8tsner VM.";
        # FIXME: --show-trace causes nix to segfault
        # command = "${nixos-generators}/bin/nixos-generate  --flake .#h8tsner --format kexec-bundle";
        command = "${nix-output-monitor}/bin/nom build $DOTFIELD_DIR#packages.h8tsner-kexec.x86_64-linux --impure";
      })

      (ci {
        name = "evalnix";
        help = "Check Nix parsing";
        command = "fd --extension nix --exec nix-instantiate --parse --quiet {} >/dev/null";
      })
      (ci {
        name = "nom-check";
        help = "Run 'nix flake check' with nom";
        command = "nix flake check --log-format internal-json -v |& ${nix-output-monitor}/bin/nom --json";
      })
      (ci {
        name = "watch-flake";
        help = "Continuously check flake";
        command = "fd . --extension=nix | entr -arc nom-check";
      })
      (ci {
        name = "flake-ci";
        help = "Show, check, then build the flake";
        command =
          let
            rebuild = if pkgs.stdenv.isLinux then "nixos-rebuild" else "darwin-rebuild";
          in
          ''
            ${nixUnstable}/bin/nix flake show --print-build-logs && \
              ${nixUnstable}/bin/nix flake check --print-build-logs && \
              ${rebuild} build --flake .#$HOSTNAME --print-build-logs
          '';
      })

      (dotfield treefmt)
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

      (utils scripts.dotfield-sync)
      (utils scripts.dotfield-rebuild)
      (utils scripts.dotfield-doom)
    ];
}
