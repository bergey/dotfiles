let
  # after https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/
  # and https://github.com/obsidiansystems/obelisk/blob/91483bab786b41eb451e7443f38341124e61244a/dep/reflex-platform/default.nix
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };
    pkgs = import nixpkgs {};   # bootstrap for config below
    in import nixpkgs {
        config = {
            allowUnfree = true;
        };
        overlays = [ (self: super: {
            haskellPackages = super.haskellPackages.override {
                overrides = (newH: oldH: rec {
                # I haven't figured out what version of Servant these need
                # keeping as an example of overlay
                # cachix = self.haskell.lib.unmarkBroken oldH.cachix;
                # cachix-api = self.haskell.lib.unmarkBroken oldH.cachix-api;
                });
            };
            nano = super.nano.overrideAttrs (oldAttrs: {
              patches = [
                (pkgs.fetchurl {
                    # fix compilation on macOS, where 'st_mtim' is unknown
                    # upstream patch not in 4.6
                    url = "https://git.savannah.gnu.org/cgit/nano.git/patch/?id=f516cddce749c3bf938271ef3182b9169ac8cbcc";
                    sha256 = "0gqymvr5vxxypr7y3sm252rsi4gjqp597l01x0lkxyvxsn45a4sx";
                })
              ];
            });
        })];
    }

