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
    flags = { bytestring-builder = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cereal"; version = "0.5.8.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Eric Mertens <emertens@galois.com>";
      author = "Lennart Kolmodin <kolmodin@dtek.chalmers.se>,\nGalois Inc.,\nLemmih <lemmih@gmail.com>,\nBas van Dijk <v.dijk.bas@gmail.com>";
      homepage = "https://github.com/GaloisInc/cereal";
      url = "";
      synopsis = "A binary serialization library";
      description = "A binary serialization library, similar to binary, that introduces an isolate\nprimitive for parser isolation, and labeled blocks for better error messages.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ (if flags.bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ]);
        buildable = true;
        };
      tests = {
        "test-cereal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cereal-0.5.8.2.tar.gz";
      sha256 = "17121355b92feea2d66220daa0ebb604a774e0d6359e2fc53bab362c44a5764f";
      });
    }) // {
    package-description-override = "name:                   cereal\r\nversion:                0.5.8.2\r\nx-revision: 1\r\nlicense:                BSD3\r\nlicense-file:           LICENSE\r\nauthor:                 Lennart Kolmodin <kolmodin@dtek.chalmers.se>,\r\n                        Galois Inc.,\r\n                        Lemmih <lemmih@gmail.com>,\r\n                        Bas van Dijk <v.dijk.bas@gmail.com>\r\nmaintainer:             Eric Mertens <emertens@galois.com>\r\ncategory:               Data, Parsing\r\nstability:              provisional\r\nbuild-type:             Simple\r\ncabal-version:          >= 1.10\r\nsynopsis:               A binary serialization library\r\nhomepage:               https://github.com/GaloisInc/cereal\r\ntested-with:            GHC == 7.2.2, GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.2\r\n\r\ndescription:\r\n  A binary serialization library, similar to binary, that introduces an isolate\r\n  primitive for parser isolation, and labeled blocks for better error messages.\r\n\r\nextra-source-files:     CHANGELOG.md\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/GaloisInc/cereal.git\r\n\r\nflag bytestring-builder\r\n  description:\r\n    Decides whether to use an older version of bytestring along with bytestring-builder or just a newer version of bytestring.\r\n    .\r\n    This flag normally toggles automatically but you can use `-fbytestring-builder` or `-f-bytestring-builder` to explicitly change it.\r\n  default: False\r\n  manual: False\r\n\r\nlibrary\r\n        default-language:       Haskell2010\r\n\r\n        build-depends:          base >= 4.4 && < 4.17, containers, array,\r\n                                ghc-prim >= 0.2\r\n\r\n        if !impl(ghc >= 8.0)\r\n          build-depends:        fail == 4.9.*\r\n\r\n        if flag(bytestring-builder)\r\n          build-depends:        bytestring >= 0.9    && < 0.10.4,\r\n                                bytestring-builder >= 0.10.4 && < 1\r\n        else\r\n          build-depends:        bytestring >= 0.10.4 && < 1\r\n\r\n        hs-source-dirs:         src\r\n\r\n        exposed-modules:        Data.Serialize,\r\n                                Data.Serialize.Put,\r\n                                Data.Serialize.Get,\r\n                                Data.Serialize.IEEE754\r\n\r\n        ghc-options:            -Wall -O2 -funbox-strict-fields\r\n\r\n\r\n\r\ntest-suite test-cereal\r\n        default-language:       Haskell2010\r\n\r\n        type:                   exitcode-stdio-1.0\r\n\r\n        build-depends:          base == 4.*,\r\n                                bytestring >= 0.9,\r\n                                QuickCheck,\r\n                                test-framework,\r\n                                test-framework-quickcheck2,\r\n                                cereal\r\n\r\n        main-is:                Main.hs\r\n        other-modules:          RoundTrip\r\n                                GetTests\r\n\r\n        hs-source-dirs:         tests\r\n";
    }