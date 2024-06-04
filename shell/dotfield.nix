{ pkgs, ... }:
let
  inherit (pkgs.stdenv) isLinux;

  inherit (pkgs)
    rage agenix ssh-to-age sops
    # sops-pgp-hook
    nvfetcher-bin deploy-rs deploy-flake nixpkgs-fmt cachix editorconfig-checker
    shellcheck shfmt terraform treefmt lefthook nix-output-monitor
    nixos-generators nix-eval-jobs nix-prefetch-git nix-build-uncached
    nixos-rebuild # For remote nixos-rebuild on darwin
    ;

    nix = pkgs.nixVersions.latest;

  inherit (pkgs.nodePackages) prettier;

  scripts = pkgs.callPackage ./scripts { };

  withCategory = category: attrset: attrset // { inherit category; };
  pkgWithCategory = category: package: { inherit package category; };

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

  commands = [
    # -- Utils --
    (utils nixos-rebuild)
    (utils nixos-generators)
    (utils nix-eval-jobs)
    (utils nix-prefetch-git)
    (utils nix-build-uncached)
    (utils deploy-rs)
    (utils deploy-flake)
    (utils terraform)
    (utils cachix)
    (utils lefthook)

    # -- Dotfield --
    (dotfield scripts.dotfield-docs)
    (withCategory "dotfield" {
      name = "repl";
      help = "Launch repl";
      command = "${nix}/bin/nix repl $PRJ_ROOT/repl.nix";
    })
    (withCategory "dotfield" {
      category = "dotfield";
      name = nvfetcher-bin.pname;
      help = nvfetcher-bin.meta.description;
      command =
        "cd $PRJ_ROOT/packages/sources; ${nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
    })
    (withCategory "dotfield" {
      name = "generate-h8tsner-kexec-bundle";
      help = "Use nixos-generate to build a kexec-build for the h8tsner VM.";
      # FIXME: --show-trace causes nix to segfault
      # command = "${nixos-generators}/bin/nixos-generate  --flake .#h8tsner --format kexec-bundle";
      command =
        "${nix-output-monitor}/bin/nom build $DOTFIELD_DIR#packages.h8tsner-kexec.x86_64-linux --impure";
    })
    # -- CI --
    (ci {
      name = "evalnix";
      help = "Check Nix parsing";
      command =
        "fd --extension nix --exec nix-instantiate --parse --quiet {} >/dev/null";
    })
    (ci {
      name = "nom-check";
      help = "Run 'nix flake check' with nom";
      command =
        "nix flake check --log-format internal-json -v |& ${nix-output-monitor}/bin/nom --json";
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
          rebuild =
            if pkgs.stdenv.isLinux then "nixos-rebuild" else "darwin-rebuild";
        in
        ''
          ${nix}/bin/nix flake show --print-build-logs && \
            ${nix}/bin/nix flake check --print-build-logs && \
            ${rebuild} build --flake .#$HOSTNAME --print-build-logs
        '';
    })

    # -- Formatter --
    (formatter treefmt)
    (formatter nixpkgs-fmt)
    (formatter prettier)
    (formatter shfmt)

    # -- Linter --
    (linter editorconfig-checker)
    (linter shellcheck)

    # -- Secrets --
    (secrets agenix)
    (secrets rage)
    (secrets ssh-to-age)
    (secrets sops)
    # (secrets sops-pgp-hook)
    {
      category = "secrets";
      name = "convert-keys";
      help = "helper to convert the usual ssh ed25519 keys to age keys";
      command = ''
        ${ssh-to-age}/bin/ssh-to-age -private-key -i ~/.ssh/id_ed25519 > ~/.config/sops/age/age-key.sec
        ${ssh-to-age}/bin/ssh-to-age -i ~/.ssh/id_ed25519.pub > ~/.config/sops/age/age-key.pub
      '';
    }
  ];
}
