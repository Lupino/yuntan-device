{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "fail"; version = "4.9.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "Herbert Valerio Riedel";
      homepage = "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail";
      url = "";
      synopsis = "Forward-compatible MonadFail class";
      description = "This package contains the \"Control.Monad.Fail\" module providing the\n<https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail MonadFail>\nclass that became available in\n<https://hackage.haskell.org/package/base-4.9.0.0 base-4.9.0.0>\nfor older @base@ package versions.\n\nThis package turns into an empty package when used with GHC versions\nwhich already provide the \"Control.Monad.Fail\" module to make way for\nGHC's own \"Control.Monad.Fail\" module.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."base" or (errorHandler.buildDepError "base"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fail-4.9.0.0.tar.gz";
      sha256 = "6d5cdb1a5c539425a9665f740e364722e1d9d6ae37fbc55f30fe3dbbbb91d4a2";
      });
    }) // {
    package-description-override = "name:                fail\nversion:             4.9.0.0\nsynopsis:            Forward-compatible MonadFail class\nhomepage:            https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Herbert Valerio Riedel\nmaintainer:          libraries@haskell.org\ncategory:            Development\nbuild-type:          Simple\ncabal-version:       >=1.10\ndescription:\n  This package contains the \"Control.Monad.Fail\" module providing the\n  <https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail MonadFail>\n  class that became available in\n  <https://hackage.haskell.org/package/base-4.9.0.0 base-4.9.0.0>\n  for older @base@ package versions.\n  .\n  This package turns into an empty package when used with GHC versions\n  which already provide the \"Control.Monad.Fail\" module to make way for\n  GHC's own \"Control.Monad.Fail\" module.\n\nlibrary\n  default-language: Haskell2010\n\n  if !impl(ghc >= 8.0)\n    exposed-modules: Control.Monad.Fail\n    build-depends:   base >=4.3 && <4.9\n";
    }