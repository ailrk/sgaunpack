{
  description = "Relic SGA tool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/48652e9d5aea46e555b3df87354280d4f29cd3a3";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs { inherit system; };

    sgaUnpackPackage = pkgs.haskellPackages.callCabal2nix "sgaunpack" ./. {};

        # Development toolchain
        devTools = with pkgs.haskellPackages; [
          ghc
          cabal-install
          haskell-language-server
        ];

        systemDeps = with pkgs; [
          zlib
          pkg-config
        ];
  in
  {
        # Allows running `nix build`
        packages.default = sgaUnpackPackage;

        # Allows running `nix run . -- unpack ...` directly
        apps.default = flake-utils.lib.mkApp {
          drv = sgaUnpackPackage;
          exePath = "/bin/sgaunpack";
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ sgaUnpackPackage ];
          buildInputs = devTools ++ systemDeps;

          shellHook = ''
          echo "----------------------------------------------------"
          echo "  Relic SGA Tool"
          echo "  GHC Version: $(ghc --version)"
          echo "  Cabal Version: $(cabal --version | head -n 1)"
          echo "----------------------------------------------------"
          '';
        };
      });
    }
