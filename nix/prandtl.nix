# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [ (self: super: {
    firejail = super.lib.overrideDerivation super.firejail (attrs: {
        postInstall = ''
    sed -E -e 's@^include (.*/)?(.*.local)$@include /etc/firejail/\2@g' -i $out/etc/firejail/*.profile
  '';
    });

  }) ];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.trustedUsers = [ "bergey" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "prandtl"; # Define your hostname.
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

    # Select internationalisation properties.
    i18n = {
        defaultLocale = "en_US.UTF-8";
    };

    console = {
        font = "Lat2-Terminus16";
        keyMap = "dvorak";
    };

  # Set your time zone.
  time.timeZone = "UTC";

#  virtualisation.xen = {
#    enable = true;
#    domain0MemorySize = 8192;
#  };
virtualisation.docker.enable = true;


  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
   environment.systemPackages = with pkgs; [
     cacert
     wget vim
     bash-completion
     # pavucontrol
   ];

   programs.firejail = {
    enable = true;
    wrappedBinaries = let inherit (pkgs.lib) getBin; in {
        chromium = "${getBin pkgs.chromium}/bin/chromium";
        darktable = "${getBin pkgs.darktable}/bin/darktable";
        firefox = "${getBin pkgs.firefox}/bin/firefox";
        gimp = "${getBin pkgs.gimp}/bin/gimp";
        krita = "${getBin pkgs.krita}/bin/krita";
        libreoffice = "${getBin pkgs.libreoffice}/bin/libreoffice";
        # slack = "${getBin pkgs.slack}/bin/slack";
        spotify = "${getBin pkgs.spotify}/bin/spotify";
        vlc = "${getBin pkgs.vlc}/bin/vlc";
        # zoom = "${getBin pkgs.zoom}/bin/zoom";
    };
   };

   environment.etc = {
     "firejail/chromium.local" = {
       mode = "0444";
       text = ''
            ignore private-dev
            '';
     };
     "firejail/firefox.local" = {
       mode = "0444";
       text = ''
            ignore private-dev
            '';
     };
   };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.ssh.startAgent = false;

  hardware.pulseaudio.enable = true;
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "dvorak";
    exportConfiguration = true;

    # Enable touchpad support.
    libinput.enable = true;

    # Enable the GNOME Desktop Environment.
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };


  services.autorandr.enable = true;

 # hardware.opengl.enable = true;
 #  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau intel-ocl intel-media-driver beignet ];

  services.udev.extraHwdb = ''
        evdev:atkbd:dmi:*            # built-in keyboard: match all AT keyboards for now
            KEYBOARD_KEY_3a=backspace     # bind capslock to backspace
            KEYBOARD_KEY_38=leftctrl   # left alt to left control
            KEYBOARD_KEY_db=leftalt # windows to left alt
            KEYBOARD_KEY_1d=leftmeta # left control to left meta
            KEYBOARD_KEY_b8=rightctrl    # right alt to right control
            KEYBOARD_KEY_b7=rightalt # print screen to right alt
            KEYBOARD_KEY_9d=esc    # right control to escape
        '';

  systemd.user.services.xcape = {
        enable = true;
        description = "xcape";
        serviceConfig = {
            PartOf = [ "graphical-session.target" ];
            ExecStart = "${pkgs.xcape}/bin/xcape -e 'Alt_L=Escape;Alt_R=Escape'";
        };
        wantedBy = [ "graphical-session.target" ];
        after = [ "graphical-session-pre.target" ];
  };

  services.keybase.enable = true;
  services.kbfs.enable = true;

  systemd.tmpfiles.rules = [ "d /tmp 1777 root root 14d" ];

  services.transmission.enable = true;

  fileSystems."/mnt/babel" = {
      label = "Babel";
      fsType = "ext4";
      options = [ "relatime" "noauto" ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
   users.extraUsers.bergey = {
     isNormalUser = true;
     uid = 1000;
     extraGroups = [ "audio" "wheel" "networkmanager" "docker" "dialout" ];
   };

  fonts.fonts = with pkgs; [
    gentium
    inconsolata
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    # noto-fonts-extra # more weights?
    # tex-gyre
  ];

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_10;
        authentication = pkgs.lib.mkOverride 10 ''
            local all all trust
            host all all ::1/128 trust
            '';
        initialScript = pkgs.writeText "bergey-initScript" ''
                      CREATE USER bergey;
                      CREATE DATABASE bergey;
                      GRANT ALL ON DATABASE bergey TO bergey;
                      '';
    };

    # Enable cron service
    services.cron = {
        enable = true;
        systemCronJobs = [
        ];
    };


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.11"; # Did you read the comment?

}
