{ config, lib, pkgs, inputs, ... }:
let
  inherit (pkgs.stdenv) isLinux isDarwin;

  inherit (config.lib.dotfield) fsPath;
  nixPath = [
    "nixpkgs=${inputs.nixpkgs}"
    (lib.optionalString isLinux "nixos-config=${fsPath}/lib/compat/nixos")
    (lib.optionalString isDarwin "darwin-config=${fsPath}/lib/compat/darwin")
  ];
in
{
  imports = [ ../../lib/system ./nix-config.nix ./system-packages.nix ];

  environment.variables = {
    DOTFIELD_DIR = "$XDG_CONFIG_HOME/dotfield";
    EDITOR = "e";
    KERNEL_NAME =
      if pkgs.stdenv.hostPlatform.isDarwin then "darwin" else "linux";
    LANG = "en_US.UTF-8";
    # LC_ALL = "en_US.UTF-8"; # don't set; breaks distrobox and other things
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    ZDOTDIR = "$HOME/.config/zsh";

    # Although it points to a commonly-used path for user-owned executables,
    # $XDG_BIN_HOME is a non-standard environment variable. It is not part of
    # the XDG Base Directory Specification.
    # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    XDG_BIN_HOME = "$HOME/.local/bin";
  };

  environment.systemPackages = lib.mkIf isLinux [
    pkgs.glibcLocales
  ];

  programs.tmux = { enable = true; };

  environment.shells = with pkgs; [ bashInteractive fish zsh ];

  # Install completions for system packages.
  environment.pathsToLink = [
    (lib.optionalString config.programs.fish.enable "/share/fish")
    (lib.optionalString config.programs.zsh.enable "/share/zsh")
  ];

  programs.zsh = {
    enable = lib.mkDefault true;
    enableCompletion = true;
    enableBashCompletion = true;
  };

  programs.bash = { enableCompletion = true; };

  programs.fish.enable = lib.mkDefault true;

  documentation = lib.mkOptionDefault {
    enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  nix = {
    inherit nixPath;
  };
}
