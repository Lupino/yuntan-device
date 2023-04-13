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
    flags = {
      two = false;
      three = false;
      four = false;
      five = false;
      five-three = true;
      mtl = true;
      generic-deriving = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "transformers-compat"; version = "0.7.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2012-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/transformers-compat/";
      url = "";
      synopsis = "A small compatibility shim for the transformers library";
      description = "This package includes backported versions of types that were added\nto transformers in transformers 0.3, 0.4, and 0.5 for users who need strict\ntransformers 0.2 or 0.3 compatibility to run on old versions of the\nplatform, but also need those types.\n\nThose users should be able to just depend on @transformers >= 0.2@\nand @transformers-compat >= 0.3@.\n\nNote: missing methods are not supplied, but this at least permits the types to be used.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ (if flags.three
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.mtl) (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ])) ++ (if flags.two
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.mtl) (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" || flags.generic-deriving) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (flags.generic-deriving) ((pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0" && flags.generic-deriving) (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-compat-0.7.1.tar.gz";
      sha256 = "ac0b861534c14d3cfd61534f474674e3b634b8decea072ffd4d21f8b59f35080";
      });
    }) // {
    package-description-override = "name:          transformers-compat\r\ncategory:      Compatibility\r\nversion:       0.7.1\r\nx-revision: 2\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/transformers-compat/\r\nbug-reports:   http://github.com/ekmett/transformers-compat/issues\r\ncopyright:     Copyright (C) 2012-2015 Edward A. Kmett\r\nsynopsis:      A small compatibility shim for the transformers library\r\ndescription:\r\n  This package includes backported versions of types that were added\r\n  to transformers in transformers 0.3, 0.4, and 0.5 for users who need strict\r\n  transformers 0.2 or 0.3 compatibility to run on old versions of the\r\n  platform, but also need those types.\r\n  .\r\n  Those users should be able to just depend on @transformers >= 0.2@\r\n  and @transformers-compat >= 0.3@.\r\n  .\r\n  Note: missing methods are not supplied, but this at least permits the types to be used.\r\n\r\nbuild-type:    Simple\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.4\r\n             , GHC == 8.10.7\r\n             , GHC == 9.0.1\r\n             , GHC == 9.2.1\r\nextra-source-files:\r\n  .ghci\r\n  .gitignore\r\n  .hlint.yaml\r\n  .vim.custom\r\n  config\r\n  tests/*.hs\r\n  tests/LICENSE\r\n  tests/transformers-compat-tests.cabal\r\n  README.markdown\r\n  CHANGELOG.markdown\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/transformers-compat.git\r\n\r\nflag two\r\n  default: False\r\n  description: Use transformers 0.2. This will be selected by cabal picking the appropriate version.\r\n  manual: False\r\n\r\nflag three\r\n  default: False\r\n  manual: False\r\n  description: Use transformers 0.3. This will be selected by cabal picking the appropriate version.\r\n\r\nflag four\r\n  default: False\r\n  manual: False\r\n  description: Use transformers 0.4. This will be selected by cabal picking the appropriate version.\r\n\r\nflag five\r\n  default: False\r\n  manual: False\r\n  description: Use transformers 0.5 up until (but not including) 0.5.3. This will be selected by cabal picking the appropriate version.\r\n\r\nflag five-three\r\n  default: True\r\n  manual: False\r\n  description: Use transformers 0.5.3. This will be selected by cabal picking the appropriate version.\r\n\r\nflag mtl\r\n  default: True\r\n  manual: True\r\n  description: -f-mtl Disables support for mtl for transformers 0.2 and 0.3. That is an unsupported configuration, and results in missing instances for `ExceptT`.\r\n\r\nflag generic-deriving\r\n  default: True\r\n  manual: True\r\n  description: -f-generic-deriving prevents generic-deriving from being built as a dependency.\r\n               This disables certain aspects of generics for older versions of GHC. In particular,\r\n               Generic(1) instances will not be backported prior to GHC 7.2, and generic operations\r\n               over unlifted types will not be backported prior to GHC 8.0. This is an unsupported\r\n               configuration.\r\n\r\nlibrary\r\n  build-depends:\r\n    base >= 4.3 && < 4.18,\r\n    -- These are all transformers versions we support.\r\n    -- each flag below splits this interval into two parts.\r\n    -- flag-true parts are mutually exclusive, so at least one have to be on.\r\n    transformers >= 0.2 && <0.7\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: fail == 4.9.*\r\n\r\n  hs-source-dirs:\r\n    src\r\n\r\n  exposed-modules:\r\n    Control.Monad.Trans.Instances\r\n\r\n  other-modules:\r\n    Paths_transformers_compat\r\n\r\n  default-language:\r\n    Haskell2010\r\n\r\n  -- automatic flags\r\n  if flag(five-three)\r\n    build-depends: transformers >= 0.5.3\r\n  else\r\n    build-depends: transformers < 0.5.3\r\n\r\n  if flag(five)\r\n    hs-source-dirs: 0.5\r\n    build-depends: transformers >= 0.5 && < 0.5.3\r\n  else\r\n    build-depends: transformers < 0.5 || >= 0.5.3\r\n\r\n  if flag(four)\r\n    cpp-options: -DTRANSFORMERS_FOUR\r\n    hs-source-dirs: 0.5\r\n    -- Don't allow transformers-0.4.0.0\r\n    -- See https://github.com/ekmett/transformers-compat/issues/35\r\n    build-depends: transformers >= 0.4.1 && < 0.5\r\n  else\r\n    build-depends: transformers < 0.4 || >= 0.5\r\n\r\n  if flag(three)\r\n    hs-source-dirs: 0.3 0.5\r\n    build-depends: transformers >= 0.3 && < 0.4\r\n    if flag(mtl)\r\n      build-depends: mtl >= 2.1 && < 2.2\r\n  else\r\n    build-depends: transformers < 0.3 || >= 0.4\r\n\r\n  if flag(two)\r\n    hs-source-dirs: 0.2 0.3 0.5\r\n    build-depends: transformers >= 0.2 && < 0.3\r\n    if flag(mtl)\r\n      build-depends: mtl >= 2.0 && < 2.1\r\n  else\r\n    build-depends: transformers >= 0.3\r\n\r\n  -- other flags\r\n  if impl(ghc >= 7.2) || flag(generic-deriving)\r\n    hs-source-dirs: generics\r\n    build-depends: ghc-prim\r\n\r\n  if flag(mtl)\r\n    cpp-options: -DMTL\r\n\r\n  if flag(generic-deriving)\r\n    if impl(ghc < 8.0) && flag(generic-deriving)\r\n      cpp-options: -DGENERIC_DERIVING\r\n      build-depends: generic-deriving >= 1.10 && < 2\r\n\r\n  if !flag(mtl) && !flag(generic-deriving)\r\n    cpp-options: -DHASKELL98\r\n\r\n  if flag(two)\r\n    exposed-modules:\r\n      Control.Applicative.Backwards\r\n      Control.Applicative.Lift\r\n      Data.Functor.Reverse\r\n\r\n  if flag(two) || flag(three)\r\n    exposed-modules:\r\n      Control.Monad.Trans.Except\r\n      Control.Monad.Signatures\r\n      Data.Functor.Classes\r\n      Data.Functor.Sum\r\n\r\n  if flag(two) || flag(three) || flag(four) || flag(five)\r\n    exposed-modules:\r\n      Control.Monad.Trans.Accum\r\n      Control.Monad.Trans.Select\r\n\r\n  if impl(ghc >= 7.2) || flag(generic-deriving)\r\n    exposed-modules:\r\n      Data.Functor.Classes.Generic\r\n      Data.Functor.Classes.Generic.Internal\r\n";
    }