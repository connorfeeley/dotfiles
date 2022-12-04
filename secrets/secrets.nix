###
### Only used by agenix CLI - this file is checked in, but shouldn't be used in the flake config
### (for organizational reasons - not for security reasons)
###
let
  cfeeley = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHCVmAx+FNqurkG9eQ7icgqS1tOzy1JyL+spWMr477mU";
  cfeeley-mac-ed = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICyCzd5UcqFUC3O7r62N3sx6ywXcayHQRV3jWJC8OQyl";
  cfeeley-rosy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICv0DIS4BFakxKLgkaQ3zn8SgMgTb4p5/XlVcASERANj";
  users = [ cfeeley cfeeley-mac-ed cfeeley-rosy ];

  workstation = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIL+myjkKGCYIYkI165tq/cp04m0iox8RLEb4MS1wjet";
  macbook-pro = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC+pW5LB+Op2HgkiCuwAOQ5UB1ATEvTrnV89CFo4toCS";
  rosy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSoMj5v8JwK4YhB8p4mrJM26TeNAO+xZgIwaxxj0Umb";
  h8tsner = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGPdaiVggrhTnMX3QmE+4UEfPAyFTdB4jJdmjNdjWFU";
  systems = [ workstation macbook-pro rosy ];
in
{
  "minecraft-rcon-password.txt.age".publicKeys = users ++ systems ++ [ h8tsner ];

  # Workstation binary cache
  "hosts/workstation/cache-priv-key.pem.age".publicKeys = users ++ [ workstation ];

  "dotfield-readme-update-access-token.txt.age".publicKeys = users ++ systems;
}
