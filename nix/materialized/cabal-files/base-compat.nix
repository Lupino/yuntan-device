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
      identifier = { name = "base-compat"; version = "0.12.1"; };
      license = "MIT";
      copyright = "(c) 2012-2018 Simon Hengel,\n(c) 2014-2018 João Cristóvão,\n(c) 2015-2018 Ryan Scott";
      maintainer = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      author = "Simon Hengel <sol@typeful.net>,\nJoão Cristóvão <jmacristovao@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "A compatibility layer for base";
      description = "Provides functions available in later versions of @base@ to\na wider range of compilers, without requiring you to use CPP\npragmas in your code.  See the\n<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown README>\nfor what is covered. Also see the\n<https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown changelog>\nfor recent changes.\n\nNote that @base-compat@ does not add any orphan instances.\nThere is a separate package,\n@<http://hackage.haskell.org/package/base-orphans base-orphans>@,\nfor that.\n\nIn addition, @base-compat@ does not backport any data types\nor type classes. See\n@<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown#data-types-and-type-classes this section of the README>@\nfor more info.\n\n@base-compat@ is designed to have zero dependencies. For a\nversion of @base-compat@ that depends on compatibility\nlibraries for a wider support window, see the\n@<http://hackage.haskell.org/package/base-compat-batteries base-compat-batteries>@\npackage. Most of the modules in this library have the same\nnames as in @base-compat-batteries@ to make it easier to\nswitch between the two. There also exist versions of each\nmodule with the suffix @.Repl@, which are distinct from\nanything in @base-compat-batteries@, to allow for easier\nuse in GHCi.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (!system.isWindows && !system.isHalvm) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base-compat-0.12.1.tar.gz";
      sha256 = "fb683cb4041b88cab1d0849f70ebd26b342c734a9ef6f75233c1602d53a015fd";
      });
    }) // {
    package-description-override = "name:             base-compat\nversion:          0.12.1\nlicense:          MIT\nlicense-file:     LICENSE\ncopyright:        (c) 2012-2018 Simon Hengel,\n                  (c) 2014-2018 João Cristóvão,\n                  (c) 2015-2018 Ryan Scott\nauthor:           Simon Hengel <sol@typeful.net>,\n                  João Cristóvão <jmacristovao@gmail.com>,\n                  Ryan Scott <ryan.gl.scott@gmail.com>\nmaintainer:       Simon Hengel <sol@typeful.net>,\n                  João Cristóvão <jmacristovao@gmail.com>,\n                  Ryan Scott <ryan.gl.scott@gmail.com>\nbuild-type:       Simple\ncabal-version:    >= 1.10\ncategory:         Compatibility\nsynopsis:         A compatibility layer for base\ndescription:      Provides functions available in later versions of @base@ to\n                  a wider range of compilers, without requiring you to use CPP\n                  pragmas in your code.  See the\n                  <https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown README>\n                  for what is covered. Also see the\n                  <https://github.com/haskell-compat/base-compat/blob/master/base-compat/CHANGES.markdown changelog>\n                  for recent changes.\n                  .\n                  Note that @base-compat@ does not add any orphan instances.\n                  There is a separate package,\n                  @<http://hackage.haskell.org/package/base-orphans base-orphans>@,\n                  for that.\n                  .\n                  In addition, @base-compat@ does not backport any data types\n                  or type classes. See\n                  @<https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown#data-types-and-type-classes this section of the README>@\n                  for more info.\n                  .\n                  @base-compat@ is designed to have zero dependencies. For a\n                  version of @base-compat@ that depends on compatibility\n                  libraries for a wider support window, see the\n                  @<http://hackage.haskell.org/package/base-compat-batteries base-compat-batteries>@\n                  package. Most of the modules in this library have the same\n                  names as in @base-compat-batteries@ to make it easier to\n                  switch between the two. There also exist versions of each\n                  module with the suffix @.Repl@, which are distinct from\n                  anything in @base-compat-batteries@, to allow for easier\n                  use in GHCi.\nextra-source-files: CHANGES.markdown, README.markdown\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-compat/base-compat\n  subdir: base-compat\n\nlibrary\n  ghc-options:\n      -Wall\n  default-language:\n      Haskell2010\n  build-depends:\n      base >= 4.3 && < 5,\n      ghc-prim\n  if !os(windows) && !os(halvm)\n      build-depends: unix\n  ghc-options:\n      -fno-warn-duplicate-exports\n  if impl(ghc >= 7.10)\n    ghc-options: -fno-warn-trustworthy-safe\n\n  hs-source-dirs:\n      src\n\n  exposed-modules:\n      Control.Concurrent.Compat\n      Control.Concurrent.MVar.Compat\n      Control.Exception.Compat\n      Control.Monad.Compat\n      Control.Monad.Fail.Compat\n      Control.Monad.IO.Class.Compat\n      Control.Monad.ST.Lazy.Unsafe.Compat\n      Control.Monad.ST.Unsafe.Compat\n      Data.Bifoldable.Compat\n      Data.Bifunctor.Compat\n      Data.Bitraversable.Compat\n      Data.Bits.Compat\n      Data.Bool.Compat\n      Data.Complex.Compat\n      Data.Either.Compat\n      Data.Foldable.Compat\n      Data.Function.Compat\n      Data.Functor.Compat\n      Data.Functor.Compose.Compat\n      Data.Functor.Const.Compat\n      Data.Functor.Contravariant.Compat\n      Data.Functor.Identity.Compat\n      Data.Functor.Product.Compat\n      Data.Functor.Sum.Compat\n      Data.IORef.Compat\n      Data.List.Compat\n      Data.List.NonEmpty.Compat\n      Data.Monoid.Compat\n      Data.Proxy.Compat\n      Data.Ratio.Compat\n      Data.Semigroup.Compat\n      Data.STRef.Compat\n      Data.String.Compat\n      Data.Tuple.Compat\n      Data.Type.Coercion.Compat\n      Data.Type.Equality.Compat\n      Data.Version.Compat\n      Data.Void.Compat\n      Data.Word.Compat\n      Debug.Trace.Compat\n      Foreign.Compat\n      Foreign.ForeignPtr.Compat\n      Foreign.ForeignPtr.Safe.Compat\n      Foreign.ForeignPtr.Unsafe.Compat\n      Foreign.Marshal.Alloc.Compat\n      Foreign.Marshal.Array.Compat\n      Foreign.Marshal.Compat\n      Foreign.Marshal.Safe.Compat\n      Foreign.Marshal.Unsafe.Compat\n      Foreign.Marshal.Utils.Compat\n      Numeric.Compat\n      Numeric.Natural.Compat\n      Prelude.Compat\n      System.Environment.Compat\n      System.Exit.Compat\n      System.IO.Compat\n      System.IO.Error.Compat\n      System.IO.Unsafe.Compat\n      Text.Read.Compat\n      Text.Read.Lex.Compat\n      Type.Reflection.Compat\n\n      Control.Concurrent.Compat.Repl\n      Control.Concurrent.MVar.Compat.Repl\n      Control.Exception.Compat.Repl\n      Control.Monad.Compat.Repl\n      Control.Monad.Fail.Compat.Repl\n      Control.Monad.IO.Class.Compat.Repl\n      Control.Monad.ST.Lazy.Unsafe.Compat.Repl\n      Control.Monad.ST.Unsafe.Compat.Repl\n      Data.Bifoldable.Compat.Repl\n      Data.Bifunctor.Compat.Repl\n      Data.Bitraversable.Compat.Repl\n      Data.Bits.Compat.Repl\n      Data.Bool.Compat.Repl\n      Data.Complex.Compat.Repl\n      Data.Either.Compat.Repl\n      Data.Foldable.Compat.Repl\n      Data.Function.Compat.Repl\n      Data.Functor.Compat.Repl\n      Data.Functor.Compose.Compat.Repl\n      Data.Functor.Const.Compat.Repl\n      Data.Functor.Contravariant.Compat.Repl\n      Data.Functor.Identity.Compat.Repl\n      Data.Functor.Product.Compat.Repl\n      Data.Functor.Sum.Compat.Repl\n      Data.IORef.Compat.Repl\n      Data.List.Compat.Repl\n      Data.List.NonEmpty.Compat.Repl\n      Data.Monoid.Compat.Repl\n      Data.Proxy.Compat.Repl\n      Data.Ratio.Compat.Repl\n      Data.Semigroup.Compat.Repl\n      Data.STRef.Compat.Repl\n      Data.String.Compat.Repl\n      Data.Tuple.Compat.Repl\n      Data.Type.Coercion.Compat.Repl\n      Data.Type.Equality.Compat.Repl\n      Data.Version.Compat.Repl\n      Data.Void.Compat.Repl\n      Data.Word.Compat.Repl\n      Debug.Trace.Compat.Repl\n      Foreign.Compat.Repl\n      Foreign.ForeignPtr.Compat.Repl\n      Foreign.ForeignPtr.Safe.Compat.Repl\n      Foreign.ForeignPtr.Unsafe.Compat.Repl\n      Foreign.Marshal.Alloc.Compat.Repl\n      Foreign.Marshal.Array.Compat.Repl\n      Foreign.Marshal.Compat.Repl\n      Foreign.Marshal.Safe.Compat.Repl\n      Foreign.Marshal.Unsafe.Compat.Repl\n      Foreign.Marshal.Utils.Compat.Repl\n      Numeric.Compat.Repl\n      Numeric.Natural.Compat.Repl\n      Prelude.Compat.Repl\n      System.Environment.Compat.Repl\n      System.Exit.Compat.Repl\n      System.IO.Compat.Repl\n      System.IO.Error.Compat.Repl\n      System.IO.Unsafe.Compat.Repl\n      Text.Read.Compat.Repl\n      Text.Read.Lex.Compat.Repl\n      Type.Reflection.Compat.Repl\n";
    }