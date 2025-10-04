{host, lib, ...}: {
  imports = [
    ../../core.nix
    ./hardware-configuration.nix
  ];

  networking.hostName = lib.mkForce "personal-workstation";
}
