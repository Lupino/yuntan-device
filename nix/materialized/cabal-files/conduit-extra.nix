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
      identifier = { name = "conduit-extra"; version = "1.3.5"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Batteries included conduit: adapters for common libraries.";
      description = "The conduit package itself maintains relative small dependencies. The purpose of this package is to collect commonly used utility functions wrapping other library dependencies, without depending on heavier-weight dependencies. The basic idea is that this package should only depend on haskell-platform packages and conduit.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "blaze" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/conduit-extra-1.3.5.tar.gz";
      sha256 = "8a648dee203c01e647fa386bfe7a5b293ce552f8b5cab9c0dd5cb71c7cd012d9";
      });
    }) // {
    package-description-override = "Name:                conduit-extra\nVersion:             1.3.5\nx-revision: 1\nSynopsis:            Batteries included conduit: adapters for common libraries.\nDescription:\n    The conduit package itself maintains relative small dependencies. The purpose of this package is to collect commonly used utility functions wrapping other library dependencies, without depending on heavier-weight dependencies. The basic idea is that this package should only depend on haskell-platform packages and conduit.\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman\nMaintainer:          michael@snoyman.com\nCategory:            Data, Conduit\nBuild-type:          Simple\nCabal-version:       >=1.8\nHomepage:            http://github.com/snoyberg/conduit\nextra-source-files:\n    test/random\n    test/filesystem/*.txt\n    test/filesystem/bin/*.txt\n    ChangeLog.md\n    README.md\n\nLibrary\n  build-depends: transformers <0.6\n\n  Exposed-modules:     Data.Conduit.Attoparsec\n                       Data.Conduit.Binary\n                       Data.Conduit.ByteString.Builder\n                       Data.Conduit.Filesystem\n                       Data.Conduit.Foldl\n                       Data.Conduit.Lazy\n                       Data.Conduit.Network\n                       Data.Conduit.Network.UDP\n                       Data.Conduit.Process\n                       Data.Conduit.Process.Typed\n                       Data.Conduit.Text\n                       Data.Conduit.Zlib\n  if !os(windows)\n      Exposed-modules: Data.Conduit.Network.Unix\n\n  if arch(x86_64) || arch(i386)\n      -- These architectures are able to perform unaligned memory accesses\n      cpp-options: -DALLOW_UNALIGNED_ACCESS\n\n  Build-depends:       base                     >= 4.9          && < 5\n                     , conduit                  >= 1.3          && < 1.4\n\n                     , bytestring               >= 0.10.2\n                     , text\n                     , transformers\n\n                     , async\n                     , attoparsec               >= 0.10\n                     , directory\n                     , filepath\n                     , network                  >= 2.3\n                     , primitive                >= 0.5\n                     , process\n                     , resourcet                >= 1.1\n                     , stm\n                     , streaming-commons        >= 0.1.16\n                     , unliftio-core\n                     , typed-process            >= 0.2.6\n\n  ghc-options:     -Wall\n\ntest-suite test\n    hs-source-dirs: test\n    main-is: Spec.hs\n    type: exitcode-stdio-1.0\n    ghc-options:   -threaded\n    cpp-options:   -DTEST\n    build-depends:   conduit\n                   , conduit-extra\n                   , base\n                   , hspec >= 1.3\n\n                   , async\n                   , attoparsec\n                   , bytestring-builder\n                   , bytestring\n                   , exceptions\n                   , process\n                   , resourcet\n                   , QuickCheck\n                   , stm\n                   , streaming-commons\n                   , text\n                   , transformers\n                   , transformers-base\n                   , directory\n                   , filepath\n    ghc-options:     -Wall\n    if os(windows)\n        cpp-options: -DWINDOWS\n    other-modules:   Data.Conduit.AttoparsecSpec\n                     Data.Conduit.BinarySpec\n                     Data.Conduit.ByteString.BuilderSpec\n                     Data.Conduit.ExtraSpec\n                     Data.Conduit.FilesystemSpec\n                     Data.Conduit.LazySpec\n                     Data.Conduit.NetworkSpec\n                     Data.Conduit.ProcessSpec\n                     Data.Conduit.Process.TypedSpec\n                     Data.Conduit.TextSpec\n                     Data.Conduit.ZlibSpec\n\nbenchmark blaze\n    type:           exitcode-stdio-1.0\n    hs-source-dirs: bench\n    build-depends:  base\n                  , conduit\n                  , conduit-extra\n                  , gauge\n                  , bytestring\n                  , bytestring-builder\n                  , transformers\n    main-is:        blaze.hs\n    ghc-options:    -Wall -O2 -rtsopts\n\nsource-repository head\n  type:     git\n  location: git://github.com/snoyberg/conduit.git\n";
    }