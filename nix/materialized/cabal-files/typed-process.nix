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
      specVersion = "1.12";
      identifier = { name = "typed-process"; version = "0.2.8.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/fpco/typed-process";
      url = "";
      synopsis = "Run external processes, with strong typing of streams";
      description = "Please see the tutorial at <https://github.com/fpco/typed-process#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ];
        buildable = true;
        };
      tests = {
        "typed-process-test" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            ];
          buildable = true;
          };
        "typed-process-test-single-threaded" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/typed-process-0.2.8.0.tar.gz";
      sha256 = "8578da545d6b2fa4b0b7296be389a736739153ced19d1dffbdee68aec978c0a9";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.34.4.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n\r\nname:           typed-process\r\nversion:        0.2.8.0\r\nx-revision: 1\r\nsynopsis:       Run external processes, with strong typing of streams\r\ndescription:    Please see the tutorial at <https://github.com/fpco/typed-process#readme>\r\ncategory:       System\r\nhomepage:       https://github.com/fpco/typed-process\r\nbug-reports:    https://github.com/fpco/typed-process/issues\r\nauthor:         Michael Snoyman\r\nmaintainer:     michael@snoyman.com\r\nlicense:        MIT\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    README.md\r\n    ChangeLog.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/fpco/typed-process\r\n\r\nlibrary\r\n  exposed-modules:\r\n      System.Process.Typed\r\n      System.Process.Typed.Internal\r\n  other-modules:\r\n      Paths_typed_process\r\n  hs-source-dirs:\r\n      src\r\n  build-depends:\r\n      async >= 2.0.1\r\n    , base >=4.12 && <5\r\n    , bytestring\r\n    , process >=1.2\r\n    , stm\r\n    , transformers\r\n    , unliftio-core\r\n  if os(windows)\r\n    cpp-options: -DWINDOWS\r\n  default-language: Haskell2010\r\n\r\ntest-suite typed-process-test\r\n  type: exitcode-stdio-1.0\r\n  main-is: Spec.hs\r\n  other-modules:\r\n      System.Process.TypedSpec\r\n      Paths_typed_process\r\n  hs-source-dirs:\r\n      test\r\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\r\n  build-depends:\r\n      async\r\n    , base >=4.12 && <5\r\n    , base64-bytestring\r\n    , bytestring\r\n    , hspec\r\n    , process >=1.2\r\n    , stm\r\n    , temporary\r\n    , transformers\r\n    , typed-process\r\n    , unliftio-core\r\n  default-language: Haskell2010\r\n\r\ntest-suite typed-process-test-single-threaded\r\n  type: exitcode-stdio-1.0\r\n  main-is: Spec.hs\r\n  other-modules:\r\n      System.Process.TypedSpec\r\n      Paths_typed_process\r\n  hs-source-dirs:\r\n      test\r\n  build-depends:\r\n      async\r\n    , base >=4.12 && <5\r\n    , base64-bytestring\r\n    , bytestring\r\n    , hspec\r\n    , process >=1.2\r\n    , stm\r\n    , temporary\r\n    , transformers\r\n    , typed-process\r\n    , unliftio-core\r\n  default-language: Haskell2010\r\n";
    }