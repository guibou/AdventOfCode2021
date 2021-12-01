with import (builtins.fetchTarball {
  sha256 = "sha256:0gqmpjavflrxwfkrj9y9i9imhfr163rcpn82mhxgb0s1n3fvrjg6";
  url = let
    rev = "af21d4126084";
    in "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) {};
with pkgs.haskellPackages;
(developPackage {
  root = ./.;

  overrides = self: super:
    {
      #weigh = super.callHackageDirect {
      #  pkg = "weigh";
      #  ver = "0.0.16";
      #  sha256 = "0icdyvxxi7493ch8xlpwn024plspbsdssxmcy5984yar298z8hcw";
      #} {};

      PyF = haskell.lib.unmarkBroken (haskell.lib.dontCheck super.PyF);
      besout = haskell.lib.unmarkBroken (haskell.lib.doJailbreak super.besout);
    };
  }).overrideAttrs(old: {
    buildInputs = [cabal-install];
  })
