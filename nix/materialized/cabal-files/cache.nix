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
      identifier = { name = "cache"; version = "0.1.3.0"; };
      license = "BSD-3-Clause";
      copyright = "2016 Henri Verroken";
      maintainer = "henriverroken@gmail.com";
      author = "Henri Verroken";
      homepage = "https://github.com/hverr/haskell-cache#readme";
      url = "";
      synopsis = "An in-memory key/value store with expiration support";
      description = "An in-memory key/value store with expiration support, similar\nto patrickmn/go-cache for Go.\n\nThe cache is a shared mutable HashMap implemented using STM and\nwith support for expiration times.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      tests = {
        "cache-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cache" or (errorHandler.buildDepError "cache"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cache-0.1.3.0.tar.gz";
      sha256 = "42e9d9f040fab2fd5fc1095a901d6348de73342b1d14254bdaf6ca3d4f11e534";
      });
    }) // {
    package-description-override = "name:           cache\nversion:        0.1.3.0\nsynopsis:       An in-memory key/value store with expiration support\nhomepage:       https://github.com/hverr/haskell-cache#readme\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Henri Verroken\nmaintainer:     henriverroken@gmail.com\ncopyright:      2016 Henri Verroken\ncategory:       Data, Cache\nbuild-type:     Simple\ncabal-version:  >= 1.10\ntested-with:    GHC == 7.10.3, GHC == 8.0.1\nbug-reports:    https://github.com/hverr/haskell-cache/issues\ndescription:\n    An in-memory key/value store with expiration support, similar\n    to patrickmn/go-cache for Go.\n    .\n    The cache is a shared mutable HashMap implemented using STM and\n    with support for expiration times.\n\nlibrary\n  ghc-options:         -Wall\n  hs-source-dirs:      src\n  exposed-modules:     Data.Cache\n                     , Data.Cache.Internal\n  build-depends:       base >= 4.8 && < 5\n                     , clock >= 0.7 && < 0.9\n                     , hashable >= 1.0.1.1\n                     , stm >= 2.2 && < 3\n                     , transformers >= 0.4.2 && < 0.6\n                     , unordered-containers >= 0.2.2 && < 0.3\n  default-language:    Haskell2010\n\ntest-suite cache-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  other-modules:       Data.CacheSpec\n  build-depends:       base\n                     , cache\n                     , clock\n                     , hspec\n                     , stm\n                     , transformers\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/hverr/cache\n";
    }