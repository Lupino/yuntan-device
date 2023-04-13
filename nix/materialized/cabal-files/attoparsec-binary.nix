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
      specVersion = "1.6";
      identifier = { name = "attoparsec-binary"; version = "0.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012 Andrew Drake";
      maintainer = "adrake@adrake.org";
      author = "Andrew Drake";
      homepage = "";
      url = "";
      synopsis = "Binary processing extensions to Attoparsec.";
      description = "This package adds a collection of helper functions to make\nthe task dealing with binary data of varying endianness from within an\nAttoparsec parser easier.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/attoparsec-binary-0.2.tar.gz";
      sha256 = "05e6445b20b396c99275de3e37bf8bb18559a5666ad5136907857bf574e77a0b";
      });
    }) // {
    package-description-override = "Name: attoparsec-binary\nVersion: 0.2\nAuthor: Andrew Drake\nCopyright: (c) 2012 Andrew Drake\nMaintainer: adrake@adrake.org\nSynopsis: Binary processing extensions to Attoparsec.\nStability: unstable\n\nLicense: BSD3\nLicense-file: LICENSE\n\nDescription: This package adds a collection of helper functions to make\n the task dealing with binary data of varying endianness from within an\n Attoparsec parser easier.\n\nCategory: Data\nBuild-type: Simple\n\nExtra-source-files: README\nCabal-version: >=1.6\n\nSource-Repository head\n  Type: git\n  Location: http://git.hax.so/pub/scm/attoparsec-binary.git\n\nLibrary\n  Exposed-modules: Data.Attoparsec.Binary\n  Build-depends:\n    attoparsec >= 0.8.5.0,\n    base >= 3 && < 5,\n    bytestring >= 0.9.1.10\n  GHC-Options: -Wall\n";
    }