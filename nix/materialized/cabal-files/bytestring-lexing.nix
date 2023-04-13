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
      identifier = { name = "bytestring-lexing"; version = "0.5.0.8"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2012–2021 wren gayle romano, 2008–2011 Don Stewart";
      maintainer = "wren@cpan.org";
      author = "wren gayle romano, Don Stewart";
      homepage = "https://wrengr.org/software/hackage.html";
      url = "";
      synopsis = "Efficiently parse and produce common integral and fractional numbers.";
      description = "The bytestring-lexing package offers extremely efficient `ByteString`\nparsers for some common lexemes: namely integral and fractional\nnumbers. In addition, it provides efficient serializers for (some\nof) the formats it parses.\n\nAs of version 0.3.0, bytestring-lexing offers the best-in-show\nparsers for integral values. (According to the Warp web server's\nbenchmark of parsing the Content-Length field of HTTP headers.) And\nas of version 0.5.0 it offers (to my knowledge) the best-in-show\nparser for fractional/floating numbers.\n\nSome benchmarks for this package can be found at:\n<https://github.com/wrengr/bytestring-lexing/tree/master/bench/html>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "test-all" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-lexing" or (errorHandler.buildDepError "bytestring-lexing"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bytestring-lexing-0.5.0.8.tar.gz";
      sha256 = "8bb3249b0adfaf18c9bd95c9221edf0354abe8e22ed06f4ec90bf550c68df3d5";
      });
    }) // {
    package-description-override = "----------------------------------------------------------------\n-- wren gayle romano <wren@cpan.org>                ~ 2021.11.02\n----------------------------------------------------------------\n\n-- Cabal >=1.10 is required by Hackage.\nCabal-Version:  >= 1.10\nBuild-Type:     Simple\n\nName:           bytestring-lexing\nVersion:        0.5.0.8\nStability:      provisional\nHomepage:       https://wrengr.org/software/hackage.html\nBug-Reports:    https://github.com/wrengr/bytestring-lexing/issues\nAuthor:         wren gayle romano, Don Stewart\nMaintainer:     wren@cpan.org\nCopyright:      Copyright (c) 2012–2021 wren gayle romano, 2008–2011 Don Stewart\nLicense:        BSD3\nLicense-File:   LICENSE\n\nCategory:       Data\nSynopsis:\n    Efficiently parse and produce common integral and fractional numbers.\nDescription:\n    The bytestring-lexing package offers extremely efficient `ByteString`\n    parsers for some common lexemes: namely integral and fractional\n    numbers. In addition, it provides efficient serializers for (some\n    of) the formats it parses.\n    .\n    As of version 0.3.0, bytestring-lexing offers the best-in-show\n    parsers for integral values. (According to the Warp web server's\n    benchmark of parsing the Content-Length field of HTTP headers.) And\n    as of version 0.5.0 it offers (to my knowledge) the best-in-show\n    parser for fractional/floating numbers.\n    .\n    Some benchmarks for this package can be found at:\n    <https://github.com/wrengr/bytestring-lexing/tree/master/bench/html>\n\n----------------------------------------------------------------\nExtra-source-files:\n    AUTHORS, CHANGELOG, README.md\n\n-- This should work as far back as GHC 7.4.1, but we don't verify that by CI.\n-- <https://github.com/wrengr/bytestring-lexing/actions?query=workflow%3Aci>\nTested-With:\n    GHC ==8.0.2,\n    GHC ==8.2.2,\n    GHC ==8.4.4,\n    GHC ==8.6.5,\n    GHC ==8.8.4,\n    GHC ==8.10.3,\n    GHC ==9.0.1,\n    GHC ==9.2.1\n\nSource-Repository head\n    Type:     git\n    Location: https://github.com/wrengr/bytestring-lexing.git\n\n----------------------------------------------------------------\nLibrary\n    Default-Language: Haskell2010\n    Ghc-Options:     -O2\n    Hs-Source-Dirs:  src\n    Exposed-Modules: Data.ByteString.Lex.Integral\n                     Data.ByteString.Lex.Fractional\n    Other-Modules:   Data.ByteString.Lex.Internal\n\n    -- These lower bounds are probably more restrictive than\n    -- necessary.  But then, we don't maintain any CI tests for\n    -- older versions, so these are the lowest bounds we've verified.\n    --\n    -- TODO(2021-10-23): bytestring 0.11.0.0 changed the internal\n    --   representation of ByteStrings to remove the offset.  While\n    --   they do offer pattern synonyms for backwards combatibility,\n    --   we should re-verify that our code doesn't depend on the details.\n    --   <https://github.com/haskell/bytestring/pull/175>\n    Build-Depends:  base              >= 4.5      && < 4.17\n                 ,  bytestring        >= 0.9.2.1  && < 0.12\n\n----------------------------------------------------------------\n-- <https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites>\n-- You can either:\n-- (1) have type:exitcode-stdio-1.0 & main-is:\n--     where main-is exports `main::IO()` as usual. Or,\n-- (2) have type:detailed-0.9 & test-module:\n--     where test-module exports tests::IO[Distribution.TestSuite.Test]\n--     and you have Build-Depends: Cabal >= 1.9.2\n--\n-- Rather than using Cabal's built-in detailed-0.9 framework, we\n-- could use the test-framework* family of packages with\n-- exitcode-stdio-1.0. cf.,\n-- <http://hackage.haskell.org/package/Decimal-0.4.2/src/Decimal.cabal> Or\n-- the tasty* family of packages with exitcode-stdio-1.0. Notice\n-- that test-framework-smallcheck is deprecated in favor of\n-- tasty-smallcheck. Both have more dependencies than Cabal, so\n-- will be harder to install on legacy systems; but then we wouldn't\n-- have to maintain our own code to glue into Cabal's detailed-0.9.\n-- Note that the oldest Tasty requires base>=4.5 whereas the oldest\n-- test-framework seems to have no lower bound on base.\n\nTest-Suite test-all\n    Default-Language: Haskell2010\n    Hs-Source-Dirs: test\n    Type:           exitcode-stdio-1.0\n    -- HACK: main-is must *not* have ./test/ like it does for executables!\n    Main-Is:        Main.hs\n    Other-Modules:  Integral\n                 ,  Fractional\n    -- We must include this library in order for the tests to use\n    -- it; but we must not give a version restriction lest Cabal\n    -- give warnings.\n    Build-Depends:  base              >= 4.5      && < 5\n                 ,  bytestring        >= 0.9.2.1  && < 0.12\n                 ,  bytestring-lexing\n                 ,  tasty             >= 0.10.1.2 && < 1.5\n                 ,  tasty-smallcheck  >= 0.8.0.1  && < 0.9\n                 ,  tasty-quickcheck  >= 0.8.3.2  && < 0.11\n                 -- QuickCheck        >= 2.10     && < 2.15\n                 -- smallcheck        >= 1.1.1    && < 1.3\n                 -- lazysmallcheck    >= 0.6      && < 0.7\n\n-- cabal configure flags:\n-- * --enable-tests\n-- * --enable-coverage (replaces the deprecated --enable-library-coverage)\n-- * --enable-benchmarks (doesn't seem to actually work... At least, I was getting errors whenever I tried passing this; maybe upping the cabal-version to 1.8 fixed that?)\n\n----------------------------------------------------------------\n----------------------------------------------------------- fin.\n";
    }