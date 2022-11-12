###
### Only used by agenix CLI - this file is checked in, but shouldn't be used in the flake config
### (for organizational reasons - not for security reasons)
###
let
  cfeeley = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL0idNvgGiucWgup/mP78zyC23uFjYq0evcWdjGQUaBH";
  cfeeley-mac-ed = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBu7vxj2IpJ5R4SL/TCbonZmAM6aFlruqc4z5zwBjyxo";
  cfeeley-yubi = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXwfYATGpZ/8EH8+i6idMaSWEW3EfgvT/cXy4zmgGbQRfHlG7jc8qokUtAy1xR4tSk8979bEzHZnBQ5LUGpu4a7W0ufd2uCg0OOxDs7lPPsxmrl4hdkn9kfw0fIdEpUej3EFuQjJcdLYT6z3zqK1KCzosa9AEiEwaflnI5+abqVhQ0q2IchYQqNxfpAIigxQ07h+EA7hJiOl8Vt9/z8Iky+iLnvdT6v1QO2XOhqD2uO+LzBThQ/5wJXsueLUw05FAe5zVCx55K1ui6HvMrgHUZ/rVSQr5X9AYvgCBwUPpY3TuyLBepHG4egccU8eFIY/uw0LFxN1Tkj91LA7mLcveVhNoWo6gIGlx6iJXidHPkZlJcAJ+eq4RNf+3gkSZ46m0p0X4hJgurMr5vTzSR4tDOSkrAgdJL6SSqNcnZZuQNg7JJDxRLrWuFup4UBGFb9/odwXa4rAgMP6dol6UhpIgVFklmbfg4FWD8YaJ1M1lVo6Jid6wVypYwpB+t13k5PdxVzjUJeOTV6jdENRE5+gk6GXoLrxYZp7u0JKmxybYcJ0U6H0azp35BKNYJaobqwtFA+3FL/pnpdRmwLWweqzZV46iO6Vq/T5r4fDxY6nc6d210VbAiFTz4HU743O30w/+3P3csu+E4LAaA8PAvJLNFPLBuMzc67mp00E1irz+Z5w==";
  cfeeley-rosy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICv0DIS4BFakxKLgkaQ3zn8SgMgTb4p5/XlVcASERANj cfeeley@rosy";
  users = [ cfeeley cfeeley-mac-ed cfeeley-yubi cfeeley-rosy ];

  workstation = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIL+myjkKGCYIYkI165tq/cp04m0iox8RLEb4MS1wjet";
  macbook-pro = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKVrybsJrANrO+FslS5MFrpMTtc2EBgXriEoF4srqQrx";
  rosy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBu7vxj2IpJ5R4SL/TCbonZmAM6aFlruqc4z5zwBjyxo cfeeley@Connors-MacBook-Pro.local";
  h8tsner = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGPdaiVggrhTnMX3QmE+4UEfPAyFTdB4jJdmjNdjWFU";
  systems = [ workstation macbook-pro rosy ];
in
{
  "minecraft-rcon-password.txt.age".publicKeys = users ++ systems ++ [ h8tsner ];
  "tailscale-luks-setup.state.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_ed25519_key.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_ed25519_key.pub.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_rsa_key.age".publicKeys = users ++ systems;
  "workstation-luks/ssh_host_rsa_key.pub.age".publicKeys = users ++ systems;
}
