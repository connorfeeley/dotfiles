{ config
, lib
, pkgs
, primaryUser
, ...
}: {
  services.openssh.enable = lib.mkForce true;
  services.openssh.permitRootLogin = lib.mkDefault "prohibit-password";
  # FIXME: too open!!! set per-host explicitly.
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;

  services.openssh.extraConfig = ''
    # Possibly fix hanging SSH multiplexes
    ClientAliveInterval 5

    StreamLocalBindUnlink yes
    AcceptEnv DISPLAY INSIDE_EMACS EMACS_VTERM_PATH
  '';
  programs.mosh.enable = true;
}
