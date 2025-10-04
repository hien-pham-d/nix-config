{host, lib, ...}: {
  imports = [
    ../../common/nixos/core.nix
    ./hardware-configuration.nix
  ];

  networking.hostName = lib.mkForce "personal-workstation";
}
