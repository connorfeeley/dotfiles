{ config, lib, primaryUser, ... }: {
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;

  services.openssh = {
    enable = lib.mkForce true;

    ports = [ (lib.our.peers.getSshPort config.networking.hostName) ];
    settings.PermitRootLogin = lib.mkDefault "prohibit-password";
    settings.X11Forwarding = true;

    # NOTE: distinct from password authentication
    settings.KbdInteractiveAuthentication = false;

    extraConfig = ''
      # Possibly fix hanging SSH multiplexes
      ClientAliveInterval 5

      StreamLocalBindUnlink yes
      AcceptEnv INSIDE_EMACS EMACS_VTERM_PATH
    '';
  };
  programs.mosh.enable = true;
}
