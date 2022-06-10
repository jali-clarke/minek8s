{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  # see https://github.com/kubernetes-client/haskell/pull/96
  inputs.kubernetes-client-haskell.url = "github:kubernetes-client/haskell/refs/pull/96/head";
  inputs.kubernetes-client-haskell.flake = false;

  outputs = { self, nixpkgs, flake-utils, kubernetes-client-haskell }:
    {
      overlays.default = final: prev:
        let
          # there is a known issue where ghc linking on M1 machines for very large modules fails due to segfault:
          # https://gitlab.haskell.org/ghc/ghc/-/issues/20369
          skipCheckIfM1 = pkg:
            if final.system == "aarch64-darwin"
              then final.haskell.lib.dontCheck pkg
              else pkg;

          overrides = haskellSelf: haskellSuper: {
            # builds just fine, not sure why hydra disagrees
            jose-jwt = final.haskell.lib.markUnbroken haskellSuper.jose-jwt;

            # need to use `callCabal2nixWithOptions` instead of `callCabal2nix` because we need the `--subpath` option.
            # without it, the build fails to pick up the `LICENSE` file for each package
            kubernetes-client =
              skipCheckIfM1 (
                haskellSelf.callCabal2nixWithOptions "kubernetes-client" kubernetes-client-haskell "--subpath=kubernetes-client" { }
              );

            # why is the subpath `kubernetes` instead of `kubernetes-client-core`? probably historical reasons
            kubernetes-client-core =
              skipCheckIfM1 (
                haskellSelf.callCabal2nixWithOptions "kubernetes-client-core" kubernetes-client-haskell "--subpath=kubernetes" { }
              );
            
            minek8s = haskellSelf.callCabal2nix "minek8s" ./. { };
          };
        in
        {
          haskellPackages = prev.haskellPackages.override { inherit overrides; };
          haskell = prev.haskell // {
            packages = builtins.mapAttrs (_: compilerPackages: compilerPackages.override { inherit overrides; }) prev.haskell.packages;
          };
        };
    } // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };

        cabalWrapped = pkgs.writeShellScriptBin "cabal" ''
          ${pkgs.hpack}/bin/hpack && exec ${pkgs.cabal-install}/bin/cabal "$@"
        '';

        format-all = pkgs.writeShellScriptBin "format-all" ''
          shopt -s globstar
          ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt . && ${pkgs.ormolu}/bin/ormolu -i {app,src,test}/**/*.hs
        '';
      in
      rec {
        packages.default = pkgs.haskellPackages.minek8s;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ packages.default.env ];
          packages = [
            cabalWrapped
            format-all
          ];
        };
      }
    );
}
