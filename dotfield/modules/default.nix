{ config, pkgs, ... }:

{
  imports = [
    ./bash.nix
    ./bat.nix
    ./direnv.nix
    ./git.nix
    ./gpg.nix
    ./gui.nix
    ./kitty.nix
    ./lorri.nix
    ./node.nix
    ./python.nix
    ./ripgrep.nix
    ./settings.nix
    ./ssh.nix
    ./tealdeer.nix
    ./zsh.nix

    ./editors/emacs
    ./espanso
  ];

  my.modules = {
    bat.enable = true;
    bash.enable = true;
    direnv.enable = true;
    espanso.enable = true;
    git.enable = true;
    gui.enable = true;
    kitty.enable = true;
    lorri.enable = true;
    node.enable = true;
    python.enable = true;
    ripgrep.enable = true;
    ssh.enable = true;
    tealdeer.enable = true;
    zsh.enable = true;

    editors = {
      emacs = {
        enable = true;
        doom.enable = true;
      };
    };
  };

  my.user.packages = with pkgs; [ nixfmt rnix-lsp ];

}
