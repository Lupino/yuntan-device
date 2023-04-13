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
      identifier = { name = "fast-logger"; version = "3.1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "https://github.com/kazu-yamamoto/logger";
      url = "";
      synopsis = "A fast logging system";
      description = "A fast logging system for Haskell";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."easy-file" or (errorHandler.buildDepError "easy-file"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.8") (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fast-logger-3.1.1.tar.gz";
      sha256 = "435f6e7e0771b9b525550c292a941ab5726b233e7d91fdeca707e3cdb531a8e7";
      });
    }) // {
    package-description-override = "Name:                   fast-logger\r\nVersion:                3.1.1\r\nx-revision: 1\r\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\r\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\r\nLicense:                BSD3\r\nLicense-File:           LICENSE\r\nSynopsis:               A fast logging system\r\nDescription:            A fast logging system for Haskell\r\nHomepage:               https://github.com/kazu-yamamoto/logger\r\nCategory:               System\r\nCabal-Version:          >= 1.10\r\nBuild-Type:             Simple\r\nExtra-Source-Files:     README.md ChangeLog.md\r\nTested-With:            GHC ==7.8.4 || ==7.10.3 || ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.3\r\n\r\nLibrary\r\n  Default-Language:     Haskell2010\r\n  GHC-Options:          -Wall\r\n  Exposed-Modules:      System.Log.FastLogger\r\n                        System.Log.FastLogger.Date\r\n                        System.Log.FastLogger.File\r\n                        System.Log.FastLogger.Internal\r\n                        System.Log.FastLogger.LoggerSet\r\n                        System.Log.FastLogger.Types\r\n  Other-Modules:        System.Log.FastLogger.Imports\r\n                        System.Log.FastLogger.IO\r\n                        System.Log.FastLogger.FileIO\r\n                        System.Log.FastLogger.LogStr\r\n                        System.Log.FastLogger.Logger\r\n  Build-Depends:        base >= 4.9 && < 5\r\n                      , array\r\n                      , auto-update >= 0.1.2\r\n                      , easy-file >= 0.2\r\n                      , bytestring >= 0.10.4\r\n                      , directory\r\n                      , filepath\r\n                      , text\r\n                      , unix-time >= 0.4.4\r\n                      , unix-compat >= 0.2\r\n  if impl(ghc < 7.8)\r\n      Build-Depends:    bytestring-builder\r\n  if impl(ghc >= 8)\r\n      Default-Extensions:  Strict StrictData\r\n\r\nTest-Suite spec\r\n  Main-Is:          Spec.hs\r\n  Hs-Source-Dirs:   test\r\n  Default-Language: Haskell2010\r\n  Type:             exitcode-stdio-1.0\r\n\r\n  Ghc-Options:      -Wall -threaded\r\n  Other-Modules:    FastLoggerSpec\r\n  Build-Tools:      hspec-discover >= 2.6\r\n  Build-Depends:    base >= 4 && < 5\r\n                  , bytestring >= 0.10.4\r\n                  , directory\r\n                  , fast-logger\r\n                  , hspec\r\n  if impl(ghc >= 8)\r\n      Default-Extensions:  Strict StrictData\r\n\r\nSource-Repository head\r\n  Type:                 git\r\n  Location:             git://github.com/kazu-yamamoto/logger.git\r\n";
    }