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
    flags = { hashable = true; binary = true; template-haskell = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "nats"; version = "1.1.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2014 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/nats/";
      url = "";
      synopsis = "Natural numbers";
      description = "Natural numbers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "7.9") ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (flags.binary) (hsPkgs."binary" or (errorHandler.buildDepError "binary"))) ++ (pkgs.lib).optional (flags.template-haskell) (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))) ++ (pkgs.lib).optional (flags.hashable) (hsPkgs."hashable" or (errorHandler.buildDepError "hashable")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nats-1.1.2.tar.gz";
      sha256 = "b9d2d85d8612f9b06f8c9bfd1acecd848e03ab82cfb53afe1d93f5086b6e80ec";
      });
    }) // {
    package-description-override = "name:          nats\r\ncategory:      Numeric, Algebra\r\nversion:       1.1.2\r\nx-revision: 4\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/nats/\r\nbug-reports:   http://github.com/ekmett/nats/issues\r\ncopyright:     Copyright (C) 2011-2014 Edward A. Kmett\r\nsynopsis:      Natural numbers\r\ndescription:   Natural numbers.\r\nbuild-type:    Simple\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.1\r\nextra-source-files:\r\n  .ghci\r\n  .gitignore\r\n  .vim.custom\r\n  .travis.yml\r\n  README.markdown\r\n  CHANGELOG.markdown\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/nats.git\r\n\r\nflag hashable\r\n  description:\r\n    You can disable the use of the `hashable` package using `-f-hashable`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply an instance of `Hashable`.\r\n  default: True\r\n  manual: True\r\n\r\nflag binary\r\n  description:\r\n    You can disable the use of the `binary` package using `-f-binary`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply an instance of `Binary`.\r\n  default: True\r\n  manual: True\r\n\r\nflag template-haskell\r\n  description:\r\n    You can disable the use of the `template-haskell` package using `-f-template-haskell`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply an instance of `Lift`.\r\n  default: True\r\n  manual: True\r\n\r\nlibrary\r\n  default-language: Haskell98\r\n  if impl(ghc<7)\r\n    -- {-# LANGUAGE DeriveDataTypeable #-} is only understood starting w/ GHC-7.0\r\n    default-extensions: DeriveDataTypeable\r\n\r\n  if impl(ghc<7.9)\r\n    -- Starting with GHC 7.10/base-4.8, \"Numeric.Natural\" lives in `base`\r\n    hs-source-dirs: src\r\n    exposed-modules: Numeric.Natural\r\n    ghc-options: -Wall\r\n\r\n    -- the needlessly relaxed bound here is to due to stack shenanigans\r\n    build-depends: base >= 2 && < 5\r\n\r\n    if flag(binary)\r\n      build-depends: binary >= 0.2 && < 0.9\r\n\r\n    if flag(template-haskell)\r\n      build-depends: template-haskell >= 2.2 && < 2.15\r\n\r\n    if flag(hashable)\r\n      build-depends: hashable >= 1.1.2.0 && < 1.5\r\n";
    }