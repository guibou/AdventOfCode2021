{
  description = "AoC 2021";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.PyF.url = "github:guibou/PyF";

  outputs = { self, nixpkgs, flake-utils, PyF}:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      with pkgs.haskell.lib;
      rec {
        defaultPackage = (pkgs.haskellPackages.developPackage
          {
            root = ./.;
            overrides = self: super:
              {
                PyF = pkgs.haskellPackages.callCabal2nix "PyF" PyF {};
                besout = unmarkBroken (doJailbreak super.besout);
              };
          });
        devShell = defaultPackage.env.overrideAttrs (old: {
          buildInputs = [ pkgs.cabal-install pkgs.haskell-language-server ];
        });
      });
}
