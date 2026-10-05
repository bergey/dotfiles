{ config, pkgs, ... }:

{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.11"; # Did you read the comment?

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

  nix = {
    settings = {
      trusted-users = [ "bergey" ];
      experimental-features = [ "nix-command" "flakes" ];
      substituters = pkgs.lib.mkBefore [
        "ssh://bergey@austenite" # TODO dedicated prandtl user
      ]; # followed by default cache.nixos.org
    };

    gc = {
      dates = "daily";
      options = "--delete-older-than 7d";
      randomizedDelaySec = "1h";
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "apfs" "zfs" ];
  boot.zfs.forceImportRoot = false;

  networking = {
    hostName = "prandtl"; # Define your hostname.
    hostId = "a9d1a9c2"; # required for ZFS
    interfaces = {
      enp0s25.useDHCP = true;
      wlp3s0.useDHCP = true;
    };
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    useDHCP = false;
    wireless = {
      enable = true;
      userControlled = true;
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

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    cacert
    wget vim
    bash-completion
  ];

  # TODO evaluate firejail, and these apps
  # why are these in system config rather than global.nix?
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

  fonts.packages = with pkgs; [
    gentium
    inconsolata
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    # noto-fonts-extra # more weights?
    # tex-gyre
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.ssh.startAgent = false;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    xkb = {
      layout = "us";
      variant = "dvorak";
    };
    exportConfiguration = true;
  };
  # Enable touchpad support.
  services.libinput.enable = true;
  programs.sway = {
    enable = true;
    extraPackages = (with pkgs; [ swaylock swayidle bemenu networkmanager]);
  };

  services.autorandr.enable = true;

  systemd.tmpfiles.rules = [ "d /tmp 1777 root root 14d" ];

  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
    ];
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16;
    extensions = with pkgs.postgresql_16.pkgs; [ pgvector ];

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

  services.caddy = {
    enable = true;
    virtualHosts."http://prandtl" = {
      serverAliases = [ "localhost" ];
      extraConfig = ''
        handle_path /memex/api/* {
          reverse_proxy localhost:8810
        }

        rewrite /prometheus /prometheus/
        handle /prometheus/* {
          reverse_proxy localhost:9001
        }

        rewrite /grafana /grafana/
        handle_path /grafana/* {
          reverse_proxy localhost:3000
        }
      '';
    };
  };

  # Observability

  services.grafana = {
    enable = true;
    settings = {
      server = {
        domain = "spaceways.home";
        http_port = 3000;
        addr = "127.0.0.1";
        root_url = "http://prandtl/grafana/";
      };
      # pre-26.05 key, because I have no secrets in grafana
      security.secret_key = "SW2YcwTIb9zpOOhoPsMm";
    };
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    webExternalUrl = "/prometheus/";
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
    globalConfig = {
      scrape_interval = "3s";
      scrape_timeout = "3s";
    };
    scrapeConfigs = [
      {
        job_name = "prandtl";
        static_configs = [{
          targets = [ "127.0.0.1:9002" ];
          labels = {
            instance = "prandtl";
          };
        }];
      }
    ];
    remoteWrite = [
      { url = "http://localhost:9099"; }
    ];
  };

  services.clickhouse.enable = true;

  services.vector = {
    enable = true;
    journaldAccess = true;
    settings = {
      api.enabled = true;
      sources = {

        journald = {
          type = "journald";
        };

        prometheus = {
          type = "prometheus_remote_write";
          address = "127.0.0.1:9099";
        };
      };

      sinks = {
        logs_out = {
          type = "clickhouse";
          inputs = ["journald"];
          endpoint = "http://localhost:8123";
          database = "default";
          table = "journald";
          skip_unknown_fields = true;
          batch = {
            max_events = 5000;
            timeout_secs = 5;
          };
          encoding = {
            timestamp_format = "unix";
          };
        };

        metrics_out = {
          type = "clickhouse";
          inputs = [ "clickhouse_metrics" ];
          endpoint = "http://localhost:8123";
          database = "default";
          table = "metrics";
          skip_unknown_fields = true;
          encoding = {
            timestamp_format = "unix";
          };
        };
      };

      transforms = {
        metrics_to_logs = {
          type = "metric_to_log";
          inputs = [ "prometheus" ];
        };

        clickhouse_metrics = {
          type = "remap";
          inputs = [ "metrics_to_logs" ];
          source = ''
.value = .gauge.value
del(.gauge)
'';
        };

        logs_not_vector = {
          type = "filter";
          inputs = [ "journald" ];
          condition = {
            type = "vrl";
            # source = ''!(._COMM == "vector" && starts_with(to_string(.message), "{"))'';
            source = ''
._COMM != "vector"
'';
          };
        };
      };
    };
  };
}
