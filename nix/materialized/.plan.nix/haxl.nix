{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { bench = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "haxl"; version = "2.4.0.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2014-present, Facebook, Inc.";
      maintainer = "The Haxl Team <haxl-team@fb.com>";
      author = "Facebook, Inc.";
      homepage = "https://github.com/facebook/Haxl";
      url = "";
      synopsis = "A Haskell library for efficient, concurrent,\nand concise data access.";
      description = "Haxl is a library and EDSL for efficient scheduling of concurrent data\naccesses with a concise applicative API.\n\nTo use Haxl, you need to implement one or more /data sources/, which\nprovide the means for accessing remote data or other I/O that you\nwant to perform using Haxl.\n\nHaxl provides two top-level modules:\n\n* /Data-source implementations/ import \"Haxl.Core\",\n\n* /Client code/ import your data sources and \"Haxl.Prelude\", or some\nother client-level API that you provide.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "readme.md" "tests/LoadCache.txt" "changelog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hashtables" or (errorHandler.buildDepError "hashtables"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Haxl/Core/Util"
          "Haxl/Core"
          "Haxl/Core/CallGraph"
          "Haxl/Core/DataCache"
          "Haxl/Core/DataSource"
          "Haxl/Core/Exception"
          "Haxl/Core/Flags"
          "Haxl/Core/Memo"
          "Haxl/Core/Monad"
          "Haxl/Core/Fetch"
          "Haxl/Core/Parallel"
          "Haxl/Core/Profile"
          "Haxl/Core/Run"
          "Haxl/Core/RequestStore"
          "Haxl/Core/ShowP"
          "Haxl/Core/StateStore"
          "Haxl/Core/Stats"
          "Haxl/Prelude"
          "Haxl/DataSource/ConcurrentIO"
          ];
        };
      exes = {
        "monadbench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = if !flags.bench then false else true;
          modules = [ "ExampleDataSource" ];
          hsSourceDirs = [ "tests" ];
          mainPath = [
            "MonadBench.hs"
            ] ++ (pkgs.lib).optional (!flags.bench) "";
          };
        "cachebench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hashtables" or (errorHandler.buildDepError "hashtables"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = if !flags.bench then false else true;
          hsSourceDirs = [ "tests" ];
          mainPath = [ "Bench.hs" ] ++ (pkgs.lib).optional (!flags.bench) "";
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hashtables" or (errorHandler.buildDepError "hashtables"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          modules = [
            "AdoTests"
            "AllTests"
            "BadDataSource"
            "BatchTests"
            "CoreTests"
            "DataCacheTest"
            "ExampleDataSource"
            "ExceptionStackTests"
            "FullyAsyncTest"
            "LoadCache"
            "MemoizationTests"
            "MockTAO"
            "MonadAsyncTest"
            "OutgoneFetchesTests"
            "ParallelTests"
            "ProfileTests"
            "SleepDataSource"
            "StatsTests"
            "TestBadDataSource"
            "TestExampleDataSource"
            "TestTypes"
            "TestUtils"
            "WorkDataSource"
            "WriteTests"
            "DataSourceDispatchTests"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "TestMain.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "2";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "2";
      rev = "minimal";
      sha256 = "";
      };
    }