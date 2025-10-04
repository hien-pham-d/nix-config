{host, ...}: {
  imports = [
    ../../core.nix
    ./hardware-configuration.nix
  ];

  boot.binfmt.emulatedSystems = [ "x86_64-linux" ];

}
