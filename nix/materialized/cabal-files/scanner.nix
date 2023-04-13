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
      identifier = { name = "scanner"; version = "0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) Yuras Shumovich 2016";
      maintainer = "shumovichy@gmail.com";
      author = "Yuras Shumovich";
      homepage = "https://github.com/Yuras/scanner";
      url = "";
      synopsis = "Fast non-backtracking incremental combinator parsing for bytestrings";
      description = "Parser combinator library designed to be fast. It doesn't\nsupport backtracking.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."scanner" or (errorHandler.buildDepError "scanner"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."scanner" or (errorHandler.buildDepError "scanner"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/scanner-0.3.1.tar.gz";
      sha256 = "53205f5a7dcb7a0547c9394ddb28a6eeb181627f006b875bfc08a88c498218d6";
      });
    }) // {
    package-description-override = "name:                scanner\nversion:             0.3.1\nsynopsis:            Fast non-backtracking incremental combinator parsing for bytestrings\nhomepage:            https://github.com/Yuras/scanner\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Yuras Shumovich\nmaintainer:          shumovichy@gmail.com\ncopyright:           (c) Yuras Shumovich 2016\ncategory:            Parsing\nbuild-type:          Simple\ncabal-version:       >=1.10\nextra-source-files:  README.md changelog.md bench/bench.png\ndescription:         Parser combinator library designed to be fast. It doesn't\n                     support backtracking.\n\nsource-repository head\n  type:                git\n  location:            git@github.com:Yuras/scanner.git\n\nlibrary\n  exposed-modules:     Scanner\n                       Scanner.Internal\n  other-modules:       Prelude\n                       Data.Either\n                       Scanner.OctetPredicates\n  build-depends:       base <5\n                     , fail\n                     , bytestring\n  hs-source-dirs:      lib, compat\n  ghc-options:         -O2\n  default-language:    Haskell2010\n\ntest-suite spec\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      spec, compat\n  main-is:             spec.hs\n  build-depends:       base\n                     , bytestring\n                     , hspec\n                     , scanner\n  other-modules:       Prelude\n                       Data.Either\n  default-language:    Haskell2010\n\nbenchmark bench\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench, examples, compat\n  main-is:             bench.hs\n  other-modules:       Redis.Reply\n                       Redis.Atto\n                       Redis.Zepto\n                       Redis.Scanner\n  default-language:    Haskell2010\n  build-depends:       base\n                     , bytestring\n                     , text\n                     , attoparsec\n                     , cereal\n                     , criterion\n                     , scanner\n";
    }