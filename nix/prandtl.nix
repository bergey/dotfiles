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

  nix.settings = {
    trusted-users = [ "bergey" ];
    experimental-features = [ "nix-command" "flakes" ];
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "apfs" "zfs" ];

  networking = {
    hostName = "prandtl"; # Define your hostname.
    hostId = "a9d1a9c2"; # required for ZFS
    interfaces = {
      # Per-interface useDHCP will be mandatory in the future, so this generated config
      # replicates the default behaviour.
      enp0s25.useDHCP = true;
      wlp3s0.useDHCP = true;
    };
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    useDHCP = false;
    wireless = {
      enable = true;
      userControlled.enable = true;
      # contains passwords, not part of public git repo
      networks = import ./wireless-networks.nix;
    };
    hosts = {
        "127.0.0.1" = [ "grafana" "prometheus" ];
    };
    # Open ports in the firewall.
    firewall.allowedTCPPorts = [ 80 ];
    # firewall.allowedUDPPorts = [ ... ];
  };

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

virtualisation.docker.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
   environment.systemPackages = with pkgs; [
     cacert
     wget vim
     bash-completion
     # pavucontrol
     xcape
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
        slack = "${getBin pkgs.slack}/bin/slack";
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


  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvorak";
    exportConfiguration = true;

    # Enable touchpad support.
    libinput.enable = true;
  };
  programs.sway = {
    enable = true;
    extraPackages = (with pkgs; [ swaylock swayidle bemenu networkmanager]);
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

  fonts.packages = with pkgs; [
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
        package = pkgs.postgresql_16;
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

  # getting some home practice with these before trying to run in the DC

  services.grafana = {
    enable = true;
    settings.server = {
      domain = "spaceways.home";
      http_port = 3000;
      addr = "127.0.0.1";
    };
  };

  # not working
  services.nginx = {
    enable = true;
    virtualHosts = {
      "prometheus" = {
        serverAliases = [ "localhost" ];
        locations."/" = {
            proxyPass = "http://localhost:9001";
          };
      };
      "grafana" = {
        locations."/" = {
          proxyPass = "http://localhost:3000/";
        };
      };
    };
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
    globalConfig = {
      scrape_interval = "10s";
      scrape_timeout = "5s";
    };
    scrapeConfigs = [
      {
        job_name = "prandtl";
        static_configs = [{
          targets = [ "127.0.0.1:9002" ];
        }];
      }
    ];
  };

  services.clickhouse.enable = true;

}
