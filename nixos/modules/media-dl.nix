{
  services.openvpn.servers = {
    pia = {
      autoStart = true;
      # Note that this is bad security practise, because the details
      # will be available in the nix store for everyone to see.
      # https://nixos.wiki/wiki/Comparison_of_secret_managing_schemes
      authUserPass = {
        username = "<redacted>";
        password = "<redacted>";
      };
      # Most of these options came from the OVPN file from the generator
      config = ''
        client
        dev tun
        proto udp
        remote ca-toronto.privacy.network 1197
        resolv-retry infinite
        nobind
        persist-key
        persist-tun
        cipher aes-256-cbc
        auth sha256
        tls-client
        remote-cert-tls server

        auth-user-pass
        compress
        verb 1
        reneg-sec 0

        # These settings was included directly in the file from
        # the generator, but I moved them to external files.
        <crl-verify>
        ${./crl.pem}
        </crl-verify>

        <ca>
        ${./ca.pem}
        </ca>

        disable-occ
      '';
    };
  };
}
