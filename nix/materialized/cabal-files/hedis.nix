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
    flags = { dev = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hedis"; version = "0.15.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2011 Falko Peters";
      maintainer = "Kostiantyn Rybnikov <k-bx@k-bx.com>";
      author = "Falko Peters <falko.peters@gmail.com>";
      homepage = "https://github.com/informatikr/hedis";
      url = "";
      synopsis = "Client library for the Redis datastore: supports full command set,\npipelining.";
      description = "Redis is an open source, advanced key-value store. It is often referred to\nas a data structure server since keys can contain strings, hashes, lists,\nsets and sorted sets. This library is a Haskell client for the Redis\ndatastore. Compared to other Haskell client libraries it has some\nadvantages:\n\n[Compatibility with Latest Stable Redis:] Hedis is intended\nto be used with the latest stable version of Redis (currently 5.0).\nMost redis commands (<http://redis.io/commands>) are available as\nhaskell functions, although MONITOR and SYNC are intentionally\nomitted. Additionally, a low-level API is\nexposed that  makes it easy for the library user to implement further\ncommands, such as new commands from an experimental Redis version.\n\n[Automatic Optimal Pipelining:] Commands are pipelined\n(<http://redis.io/topics/pipelining>) as much as possible without any\nwork by the user. See\n<http://informatikr.com/2012/redis-pipelining.html> for a\ntechnical explanation of automatic optimal pipelining.\n\n[Enforced Pub\\/Sub semantics:] When subscribed to the Redis Pub\\/Sub server\n(<http://redis.io/topics/pubsub>), clients are not allowed to issue\ncommands other than subscribing to or unsubscribing from channels. This\nlibrary uses the type system to enforce the correct behavior.\n\n[Connect via TCP or Unix Domain Socket:] TCP sockets are the default way to\nconnect to a Redis server. For connections to a server on the same\nmachine, Unix domain sockets offer higher performance than the standard\nTCP connection.\n\nFor detailed documentation, see the \"Database.Redis\" module.\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."scanner" or (errorHandler.buildDepError "scanner"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-lexing" or (errorHandler.buildDepError "bytestring-lexing"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
          (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "hedis-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "hedis-test-cluster" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "hedis-benchmark" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hedis-0.15.1.tar.gz";
      sha256 = "15d1f131ab11ea133d90fc090565700805c992a94b14124ee84cfe8a838809a8";
      });
    }) // {
    package-description-override = "name:               hedis\nversion:            0.15.1\nx-revision: 1\nsynopsis:\n    Client library for the Redis datastore: supports full command set,\n    pipelining.\nDescription:\n    Redis is an open source, advanced key-value store. It is often referred to\n    as a data structure server since keys can contain strings, hashes, lists,\n    sets and sorted sets. This library is a Haskell client for the Redis\n    datastore. Compared to other Haskell client libraries it has some\n    advantages:\n    .\n    [Compatibility with Latest Stable Redis:] Hedis is intended\n        to be used with the latest stable version of Redis (currently 5.0).\n    Most redis commands (<http://redis.io/commands>) are available as\n    haskell functions, although MONITOR and SYNC are intentionally\n    omitted. Additionally, a low-level API is\n        exposed that  makes it easy for the library user to implement further\n        commands, such as new commands from an experimental Redis version.\n    .\n    [Automatic Optimal Pipelining:] Commands are pipelined\n        (<http://redis.io/topics/pipelining>) as much as possible without any\n        work by the user. See\n        <http://informatikr.com/2012/redis-pipelining.html> for a\n        technical explanation of automatic optimal pipelining.\n    .\n    [Enforced Pub\\/Sub semantics:] When subscribed to the Redis Pub\\/Sub server\n        (<http://redis.io/topics/pubsub>), clients are not allowed to issue\n        commands other than subscribing to or unsubscribing from channels. This\n        library uses the type system to enforce the correct behavior.\n    .\n    [Connect via TCP or Unix Domain Socket:] TCP sockets are the default way to\n        connect to a Redis server. For connections to a server on the same\n        machine, Unix domain sockets offer higher performance than the standard\n        TCP connection.\n    .\n    For detailed documentation, see the \"Database.Redis\" module.\n    .\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Falko Peters <falko.peters@gmail.com>\nmaintainer:         Kostiantyn Rybnikov <k-bx@k-bx.com>\ncopyright:          Copyright (c) 2011 Falko Peters\ncategory:           Database\nbuild-type:         Simple\ncabal-version:      >=1.10\nhomepage:           https://github.com/informatikr/hedis\nbug-reports:        https://github.com/informatikr/hedis/issues\nextra-source-files: CHANGELOG\n\nsource-repository head\n  type:     git\n  location: https://github.com/informatikr/hedis\n\nflag dev\n  description: enable this for local development -Werror and profiling options\n  default: False\n  manual: True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall -fwarn-tabs\n  if impl(ghc >= 8.6.0)\n    ghc-options:    -Wno-warnings-deprecations\n  if flag(dev)\n    ghc-options:    -Werror\n  if flag(dev)\n    ghc-prof-options: -auto-all\n  exposed-modules:  Database.Redis\n                  , Database.Redis.Sentinel\n                  , Database.Redis.Core.Internal\n  build-depends:    scanner >= 0.2,\n                    async >= 2.1,\n                    base >= 4.8 && < 5,\n                    bytestring >= 0.9,\n                    bytestring-lexing >= 0.5,\n                    exceptions,\n                    unordered-containers,\n                    containers,\n                    text,\n                    deepseq,\n                    mtl >= 2,\n                    network >= 2 && < 3.2,\n                    resource-pool >= 0.2.2,\n                    stm,\n                    time,\n                    tls >= 1.3,\n                    vector >= 0.9,\n                    HTTP,\n                    errors,\n                    network-uri,\n                    unliftio-core\n  if !impl(ghc >= 8.0)\n    build-depends:\n      semigroups >= 0.11 && < 0.19\n\n  other-modules:    Database.Redis.Core,\n                    Database.Redis.Connection,\n                    Database.Redis.Cluster,\n                    Database.Redis.Cluster.HashSlot,\n                    Database.Redis.Cluster.Command,\n                    Database.Redis.ProtocolPipelining,\n                    Database.Redis.Protocol,\n                    Database.Redis.PubSub,\n                    Database.Redis.Transactions,\n                    Database.Redis.Types\n                    Database.Redis.Commands,\n                    Database.Redis.ManualCommands,\n                    Database.Redis.URL,\n                    Database.Redis.ConnectionContext\n  other-extensions: StrictData\n\nbenchmark hedis-benchmark\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: benchmark/Benchmark.hs\n    build-depends:\n        base == 4.*,\n        mtl >= 2.0,\n        hedis,\n        time >= 1.2\n    ghc-options: -O2 -Wall -rtsopts\n    if flag(dev)\n      ghc-options: -Werror\n    if flag(dev)\n      ghc-prof-options: -auto-all\n\ntest-suite hedis-test\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test\n    main-is: Main.hs\n    other-modules: PubSubTest\n                   Tests\n    build-depends:\n        base == 4.*,\n        bytestring >= 0.10,\n        hedis,\n        HUnit,\n        async,\n        stm,\n        text,\n        mtl == 2.*,\n        test-framework,\n        test-framework-hunit,\n        time\n    -- We use -O0 here, since GHC takes *very* long to compile so many constants\n    ghc-options: -O0 -Wall -rtsopts -fno-warn-unused-do-bind\n    if flag(dev)\n      ghc-options: -Werror\n    if flag(dev)\n      ghc-prof-options: -auto-all\n\ntest-suite hedis-test-cluster\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test\n    main-is: ClusterMain.hs\n    other-modules: PubSubTest\n                   Tests\n    build-depends:\n        base == 4.*,\n        bytestring >= 0.10,\n        hedis,\n        HUnit,\n        async,\n        stm,\n        text,\n        mtl == 2.*,\n        test-framework,\n        test-framework-hunit,\n        time\n    -- We use -O0 here, since GHC takes *very* long to compile so many constants\n    ghc-options: -O0 -Wall -rtsopts -fno-warn-unused-do-bind\n    if flag(dev)\n      ghc-options: -Werror\n    if flag(dev)\n      ghc-prof-options: -auto-all\n\ntest-suite doctest\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: DocTest.hs\n    ghc-options: -O0 -rtsopts\n    build-depends:\n        base == 4.*,\n        doctest\n";
    }