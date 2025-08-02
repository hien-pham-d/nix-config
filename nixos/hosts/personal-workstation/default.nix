{host, ...}: {
  imports = [
    ../../core.nix
    ./custom.nix
    ./todo-hardware-configuration.nix
  ];
}
