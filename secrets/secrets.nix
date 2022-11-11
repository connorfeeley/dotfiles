###
### Only used by agenix CLI - this file is checked in, but shouldn't be used in the flake config
### (for organizational reasons - not for security reasons)
###
let
  cfeeley = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL0idNvgGiucWgup/mP78zyC23uFjYq0evcWdjGQUaBH";
  users = [ cfeeley ];

  workstation = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIL+myjkKGCYIYkI165tq/cp04m0iox8RLEb4MS1wjet";
  macbook-pro = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKVrybsJrANrO+FslS5MFrpMTtc2EBgXriEoF4srqQrx";
  systems = [ workstation macbook-pro ];
in
{
  "minecraft-rcon-password.txt.age".publicKeys = [ cfeeley macbook-pro ];
  "tailscale-luks-setup.state.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_ed25519_key.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_ed25519_key.pub.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_rsa_key.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_rsa_key.pub.age".publicKeys = users ++ systems;
}
