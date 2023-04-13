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
      specVersion = "1.8";
      identifier = { name = "network-conduit-tls"; version = "1.3.2"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Create TLS-aware network code with conduits";
      description = "Uses the tls package for a pure-Haskell implementation.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-conduit-tls" or (errorHandler.buildDepError "network-conduit-tls"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-conduit-tls-1.3.2.tar.gz";
      sha256 = "ecfd60e162de3993a71906293dcf2ec8bd4c794471eb8dca13746c1d8fd3ad7f";
      });
    }) // {
    package-description-override = "name:                network-conduit-tls\nversion:             1.3.2\nsynopsis:            Create TLS-aware network code with conduits\ndescription:         Uses the tls package for a pure-Haskell implementation.\nhomepage:            https://github.com/snoyberg/conduit\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Michael Snoyman\nmaintainer:          michael@snoyman.com\ncategory:            Network\nbuild-type:          Simple\ncabal-version:       >=1.8\nextra-source-files:  README.md ChangeLog.md\n\nlibrary\n  exposed-modules:    Data.Conduit.Network.TLS\n                      Data.Conduit.Network.TLS.Internal\n  build-depends:      base            >= 4.9      && < 5\n                    , bytestring      >= 0.9\n                    , tls             >= 1.3\n                    , conduit-extra   >= 1.3\n                    , conduit         >= 1.3\n                    , network\n                    , transformers\n                    , connection\n                    , streaming-commons >= 0.1.12\n                    , unliftio-core\n                    , data-default-class\n\ntest-suite test\n    hs-source-dirs: test\n    main-is: main.hs\n    type: exitcode-stdio-1.0\n    cpp-options:   -DTEST\n    build-depends:   conduit\n                   , conduit-extra\n                   , connection\n                   , base\n                   , mtl\n                   , network-conduit-tls\n                   , bytestring\n                   , HUnit\n    ghc-options:     -Wall -threaded\n";
    }