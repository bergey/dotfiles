{ config, pkgs, ... }:
# run with ./result/bin/run-nixos-vm -nographic
# exit with C-a x

{
  system.stateVersion = "26.05";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.users.bergey = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    initialPassword = "pass";
  };

  environment.systemPackages = with pkgs; [
    claude-code
    cargo
    rustc
    (rocq-core.withPackages (ps: with ps; [
      stdlib
    ]))
  ];

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (pkgs.lib.getName pkg) [
      "claude-code"
    ];

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

}
