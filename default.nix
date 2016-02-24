{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, haskell-src-exts, hint
      , pretty-show, servant-server, stdenv, text, transformers, wai
      , warp
      }:
      mkDerivation {
        pname = "typegrams";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base haskell-src-exts hint pretty-show ];
        executableHaskellDepends = [
          aeson base servant-server text transformers wai warp
        ];
        homepage = "http://github.com/sleexyz/typegrams";
        description = "Type visualizations";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
