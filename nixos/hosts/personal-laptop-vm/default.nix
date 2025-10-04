{host, lib, ...}: {
  imports = [
    ../../core.nix
    ./hardware-configuration.nix
  ];

  networking.hostName = lib.mkForce "personal-laptop-vm";

  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
}
