{host, lib, ...}: {
  imports = [
    ../../core.nix
    ./hardware-configuration.nix
  ];

  networking.hostName = lib.mkForce "work-laptop-vm";

  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
}
