{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, haskell-src-exts, hint, pretty-show
      , stdenv
      }:
      mkDerivation {
        pname = "typegrams";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base haskell-src-exts hint pretty-show ];
        executableHaskellDepends = [ base ];
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
