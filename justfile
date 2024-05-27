#!/usr/bin/env -S just --justfile
# ^ A shebang isn't required, but allows a justfile to be executed
#   like a script, with `./justfile test`, for example.

set shell := ["zsh", "-cu"]

# Use direnv
set dotenv-load

# All arguments are exported when the export setting is set:
set export

alias b := build

bt := '0'

export RUST_BACKTRACE := bt

log := "warn"
export JUST_LOG := log

# fzf prompt
default:
  @just --choose

nixos-rebuild *ARGS:
  sudo nixos-rebuild --print-build-logs --keep-going --show-trace --verbose {{ARGS}}

# build current host
build *ARGS:
  nom build .#{{if os() == "linux" { "nixos"  } else { "darwin"  } }}Configurations.$(hostname).config.system.build.toplevel {{ARGS}}

# switch current host
switch: build
  sudo nix-env -p /nix/var/nix/profiles/system --set $(readlink ./result)
  sudo /nix/var/nix/profiles/system/activate

test:
  echo {{if os() == "linux" { "sudo nixos"  } else { "darwin"  } }}

deploy host="cfeeley-laptop":
  echo "Deploying to {{host}}"
  deploy --skip-checks --remote-build .#cfeeley-laptop -- --show-trace --print-build-logs --verbose

deploy-workstation action="build":
  echo "Running nixos-rebuild {{action}} on workstation"
  nixos-rebuild {{action}} --fast --target-host root@workstation --build-host root@workstation --flake ".#workstation"
