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

# build current host
build:
  {{if os() == "linux" { "nixos"  } else { "darwin"  } }}-rebuild build --verbose --print-build-logs --show-trace --flake .#$(hostname)

# switch current host
switch:
  {{if os() == "linux" { "sudo nixos"  } else { "darwin"  } }}-rebuild switch --verbose --print-build-logs --show-trace --flake .#$(hostname)

test:
  echo {{if os() == "linux" { "sudo nixos"  } else { "darwin"  } }}

deploy host="cfeeley-laptop":
  echo "Deploying to {{host}}"
  deploy --skip-checks --remote-build .#cfeeley-laptop -- --show-trace --print-build-logs --verbose
