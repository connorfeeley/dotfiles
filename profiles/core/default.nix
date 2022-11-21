{ self
, config
, lib
, pkgs
, profiles
, inputs
, ...
}:
let
  inherit (config.lib) dotfield;

  inherit (pkgs.stdenv) isDarwin;

  nix-doc-path =
    if isDarwin then "${pkgs.nix-doc}/lib/libnix_doc_plugin.dylib"
    else "${pkgs.nix-doc}/lib/libnix_doc_plugin.so";
in
{
  imports = [
    ../../lib/system
    ./nix-config.nix
    ./system-packages.nix
  ];

  # TODO: can this be merged with the 'dotfield' lib?
  lib.our = self.lib;

  # Load nix-doc pluign
  nix.settings.plugin-files = nix-doc-path; # Plugins to be loaded by nix
  environment.systemPackages = [ pkgs.nix-doc ];

  environment.variables = {
    DOTFIELD_DIR = dotfield.fsPath;
    EDITOR = "e";
    KERNEL_NAME =
      if pkgs.stdenv.hostPlatform.isDarwin
      then "darwin"
      else "linux";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
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

  programs.tmux = {
    enable = true;
    enableSensible = true;
  };

  environment.shells = with pkgs; [
    bashInteractive
    fish
    zsh
  ];

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

  programs.bash = {
    enableCompletion = true;
  };

  programs.fish.enable = lib.mkDefault true;

  documentation = {
    enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  };

  # Enable info and man pages
  programs = {
    info.enable = true;
    # Include "man" outputs of all systemPackages
    man.enable = true;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
