{
  description = "AoC 2021";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils}:
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
                PyF = unmarkBroken (dontCheck super.PyF);
                besout = unmarkBroken (doJailbreak super.besout);
              };
          });
        devShell = defaultPackage.env.overrideAttrs (old: {
          buildInputs = [ pkgs.cabal-install pkgs.haskell-language-server ];
        });
      });
}
