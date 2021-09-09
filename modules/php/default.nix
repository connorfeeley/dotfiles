{ config, pkgs, lib, ... }:
with lib;
let cfg = config.my.modules.php;
in {
  options = { my.modules.php = { enable = mkEnableOption false; }; };

  config = mkIf cfg.enable {
    my.user.packages = with pkgs; [
      php74
      phpPackages.composer
      # phpPackages.phpcs
      # phpPackages.php-cs-fixer
      # php80Packages.psalm
      # wp-cli

      # Generated by https://github.com/stephank/composer-plugin-nixify
      (callPackage ../../composer-project.nix { } ../../.)

      # Get the current `php` executable's version number.
      (writeShellScriptBin "php-version" "php -v | awk '/^PHP/{print $2}'")
    ];

  };
}
