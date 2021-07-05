{
  pkgs = hackage:
    {
      packages = {
        "wai".revision = (((hackage."wai")."3.2.3").revisions).default;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.2.1").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "old-time".revision = (((hackage."old-time")."1.1.0.3").revisions).default;
        "hslogger".revision = (((hackage."hslogger")."1.3.1.0").revisions).default;
        "hslogger".flags.network--gt-3_0_0 = true;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.11").revisions).default;
        "bifunctors".flags.tagged = true;
        "bifunctors".flags.semigroups = true;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "hashtables".revision = (((hackage."hashtables")."1.2.4.1").revisions).default;
        "hashtables".flags.portable = false;
        "hashtables".flags.detailed-profiling = false;
        "hashtables".flags.unsafe-tricks = true;
        "hashtables".flags.bounds-checking = false;
        "hashtables".flags.debug = false;
        "hashtables".flags.sse42 = false;
        "cryptohash-sha1".revision = (((hackage."cryptohash-sha1")."0.11.100.1").revisions).default;
        "socks".revision = (((hackage."socks")."0.6.1").revisions).default;
        "warp".revision = (((hackage."warp")."3.3.16").revisions).default;
        "warp".flags.network-bytestring = false;
        "warp".flags.allow-sendfilefd = true;
        "warp".flags.warp-debug = false;
        "Only".revision = (((hackage."Only")."0.1").revisions).default;
        "cookie".revision = (((hackage."cookie")."0.4.5").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.2.1").revisions).default;
        "cache".revision = (((hackage."cache")."0.1.3.0").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.11.2").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "test-framework-hunit".revision = (((hackage."test-framework-hunit")."0.3.0.2").revisions).default;
        "test-framework-hunit".flags.base4 = true;
        "test-framework-hunit".flags.base3 = false;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.5").revisions).default;
        "time-compat".flags.old-locale = false;
        "cereal".revision = (((hackage."cereal")."0.5.8.1").revisions).default;
        "cereal".flags.bytestring-builder = false;
        "x509-system".revision = (((hackage."x509-system")."1.6.6").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.5.0").revisions).default;
        "yaml".flags.no-exe = true;
        "yaml".flags.no-examples = true;
        "xml".revision = (((hackage."xml")."1.3.14").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        "psqueues".revision = (((hackage."psqueues")."0.2.7.2").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.15.1").revisions).default;
        "conduit-extra".revision = (((hackage."conduit-extra")."1.3.5").revisions).default;
        "fast-logger".revision = (((hackage."fast-logger")."3.0.5").revisions).default;
        "call-stack".revision = (((hackage."call-stack")."0.3.0").revisions).default;
        "simple-sendfile".revision = (((hackage."simple-sendfile")."0.2.30").revisions).default;
        "simple-sendfile".flags.allow-bsd = true;
        "safe".revision = (((hackage."safe")."0.3.19").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "http-date".revision = (((hackage."http-date")."0.0.11").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.7").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "vector".revision = (((hackage."vector")."0.12.3.0").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.boundschecks = true;
        "vector".flags.wall = false;
        "conduit".revision = (((hackage."conduit")."1.3.4.1").revisions).default;
        "word8".revision = (((hackage."word8")."0.1.3").revisions).default;
        "network".revision = (((hackage."network")."3.1.1.1").revisions).default;
        "HUnit".revision = (((hackage."HUnit")."1.6.2.0").revisions).default;
        "http-types".revision = (((hackage."http-types")."0.12.3").revisions).default;
        "scotty".revision = (((hackage."scotty")."0.12").revisions).default;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "wai-logger".revision = (((hackage."wai-logger")."2.3.6").revisions).default;
        "async".revision = (((hackage."async")."2.2.3").revisions).default;
        "async".flags.bench = false;
        "net-mqtt".revision = (((hackage."net-mqtt")."0.7.0.1").revisions).default;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "auto-update".revision = (((hackage."auto-update")."0.1.6").revisions).default;
        "monad-control".revision = (((hackage."monad-control")."1.0.2.3").revisions).default;
        "random".revision = (((hackage."random")."1.2.0").revisions).default;
        "errors".revision = (((hackage."errors")."2.3.0").revisions).default;
        "unix-compat".revision = (((hackage."unix-compat")."0.5.3").revisions).default;
        "unix-compat".flags.old-time = false;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.16.1.0").revisions).default;
        "optparse-applicative".flags.process = true;
        "time-manager".revision = (((hackage."time-manager")."0.0.0").revisions).default;
        "network-conduit-tls".revision = (((hackage."network-conduit-tls")."1.3.2").revisions).default;
        "bytestring-builder".revision = (((hackage."bytestring-builder")."0.10.8.2.0").revisions).default;
        "bytestring-builder".flags.bytestring_has_builder = true;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.old-random = false;
        "QuickCheck".flags.templatehaskell = true;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.5").revisions).default;
        "scientific".revision = (((hackage."scientific")."0.3.7.0").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "resource-pool".revision = (((hackage."resource-pool")."0.2.3.2").revisions).default;
        "resource-pool".flags.developer = false;
        "entropy".revision = (((hackage."entropy")."0.4.1.6").revisions).default;
        "entropy".flags.halvm = false;
        "tls".revision = (((hackage."tls")."1.5.5").revisions).default;
        "tls".flags.compat = true;
        "tls".flags.network = true;
        "tls".flags.hans = false;
        "distributive".revision = (((hackage."distributive")."0.6.2.1").revisions).default;
        "distributive".flags.tagged = true;
        "distributive".flags.semigroups = true;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.5.2").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "wai-extra".revision = (((hackage."wai-extra")."3.1.6").revisions).default;
        "wai-extra".flags.build-example = false;
        "iproute".revision = (((hackage."iproute")."1.7.11").revisions).default;
        "cryptohash-md5".revision = (((hackage."cryptohash-md5")."0.11.100.1").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.4").revisions).default;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.bench = true;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.properties = true;
        "base".revision = (((hackage."base")."4.14.2.0").revisions).default;
        "postgresql-simple".revision = (((hackage."postgresql-simple")."0.6.4").revisions).default;
        "postgresql-libpq".revision = (((hackage."postgresql-libpq")."0.9.4.3").revisions).default;
        "postgresql-libpq".flags.use-pkg-config = true;
        "network-uri".revision = (((hackage."network-uri")."2.6.4.1").revisions).default;
        "regex-posix".revision = (((hackage."regex-posix")."0.96.0.0").revisions).default;
        "regex-posix".flags._regex-posix-clib = false;
        "rts".revision = (((hackage."rts")."1.0.1").revisions).default;
        "text".revision = (((hackage."text")."1.2.4.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "uuid".revision = (((hackage."uuid")."1.3.15").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.14.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "easy-file".revision = (((hackage."easy-file")."0.2.2").revisions).default;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.1").revisions).default;
        "aeson-result".revision = (((hackage."aeson-result")."0.1.0.0").revisions).default;
        "typed-process".revision = (((hackage."typed-process")."0.2.6.0").revisions).default;
        "vault".revision = (((hackage."vault")."0.3.1.5").revisions).default;
        "vault".flags.useghc = true;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "network-byte-order".revision = (((hackage."network-byte-order")."0.1.6").revisions).default;
        "unliftio".revision = (((hackage."unliftio")."0.2.18").revisions).default;
        "fail".revision = (((hackage."fail")."4.9.0.0").revisions).default;
        "test-framework".revision = (((hackage."test-framework")."0.8.2.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.12.0").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "hostname".revision = (((hackage."hostname")."1.0").revisions).default;
        "basement".revision = (((hackage."basement")."0.0.12").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "x509".revision = (((hackage."x509")."1.7.5").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.4.1").revisions).default;
        "regex-compat".revision = (((hackage."regex-compat")."0.95.2.1").revisions).default;
        "psql-utils".revision = (((hackage."psql-utils")."0.2.0.1").revisions).default;
        "byteorder".revision = (((hackage."byteorder")."1.0.4").revisions).default;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.deepseq = true;
        "tagged".flags.transformers = true;
        "scotty-utils".revision = (((hackage."scotty-utils")."0.1.0.0").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.4").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.7.1.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.6").revisions).default;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.three = false;
        "transformers-compat".flags.mtl = true;
        "memory".revision = (((hackage."memory")."0.15.0").revisions).default;
        "memory".flags.support_basement = true;
        "memory".flags.support_bytestring = true;
        "memory".flags.support_foundation = true;
        "memory".flags.support_deepseq = true;
        "bsb-http-chunked".revision = (((hackage."bsb-http-chunked")."0.0.0.4").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.4.2.0").revisions).default;
        "scanner".revision = (((hackage."scanner")."0.3.1").revisions).default;
        "resourcet".revision = (((hackage."resourcet")."1.2.4.2").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.5.6.0").revisions).default;
        "aeson".flags.developer = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.fast = false;
        "aeson".flags.cffi = false;
        "cabal-doctest".revision = (((hackage."cabal-doctest")."1.0.8").revisions).default;
        "connection".revision = (((hackage."connection")."0.3.1").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.14.0").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.5").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.2").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "scotty-haxl".revision = (((hackage."scotty-haxl")."0.1.0.0").revisions).default;
        "network-bsd".revision = (((hackage."network-bsd")."2.8.1.0").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.3").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "x509-store".revision = (((hackage."x509-store")."1.6.7").revisions).default;
        "websockets".revision = (((hackage."websockets")."0.12.7.2").revisions).default;
        "websockets".flags.example = false;
        "hedis".revision = (((hackage."hedis")."0.14.4").revisions).default;
        "hedis".flags.dev = false;
        "bytestring-lexing".revision = (((hackage."bytestring-lexing")."0.5.0.2").revisions).default;
        "asn1-types".revision = (((hackage."asn1-types")."0.3.4").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "asn1-encoding".revision = (((hackage."asn1-encoding")."0.9.6").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "strict".revision = (((hackage."strict")."0.4.0.1").revisions).default;
        "strict".flags.assoc = true;
        "HTTP".revision = (((hackage."HTTP")."4000.3.16").revisions).default;
        "HTTP".flags.mtl1 = false;
        "HTTP".flags.conduit10 = false;
        "HTTP".flags.warp-tests = false;
        "HTTP".flags.warn-as-error = false;
        "HTTP".flags.network-uri = true;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.5").revisions).default;
        "attoparsec".flags.developer = false;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "nats".revision = (((hackage."nats")."1.1.2").revisions).default;
        "nats".flags.binary = true;
        "nats".flags.template-haskell = true;
        "nats".flags.hashable = true;
        "pem".revision = (((hackage."pem")."0.2.4").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "rediscaching-haxl".revision = (((hackage."rediscaching-haxl")."0.1.0.0").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.examples = false;
        "clock".revision = (((hackage."clock")."0.8.2").revisions).default;
        "clock".flags.llvm = false;
        "comonad".revision = (((hackage."comonad")."5.0.8").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.indexed-traversable = true;
        "comonad".flags.containers = true;
        "aeson-helper".revision = (((hackage."aeson-helper")."0.1.0.0").revisions).default;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "asn1-parse".revision = (((hackage."asn1-parse")."0.9.5").revisions).default;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.1.0.0").revisions).default;
        "regex-base".revision = (((hackage."regex-base")."0.94.0.1").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.2.1.0").revisions).default;
        "cryptonite".revision = (((hackage."cryptonite")."0.29").revisions).default;
        "cryptonite".flags.old_toolchain_inliner = false;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_aesni = true;
        "cryptonite".flags.use_target_attributes = true;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.check_alignment = false;
        "network-info".revision = (((hackage."network-info")."0.2.0.10").revisions).default;
        "th-compat".revision = (((hackage."th-compat")."0.1.2").revisions).default;
        "indexed-traversable".revision = (((hackage."indexed-traversable")."0.1.1").revisions).default;
        "zlib".revision = (((hackage."zlib")."0.6.2.3").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "attoparsec-binary".revision = (((hackage."attoparsec-binary")."0.2").revisions).default;
        "hourglass".revision = (((hackage."hourglass")."0.2.12").revisions).default;
        "appar".revision = (((hackage."appar")."0.1.8").revisions).default;
        "http2".revision = (((hackage."http2")."3.0.2").revisions).default;
        "http2".flags.devel = false;
        "http2".flags.h2spec = false;
        "http2".flags.doc = false;
        "unix-time".revision = (((hackage."unix-time")."0.4.7").revisions).default;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.11.2").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "SHA".revision = (((hackage."SHA")."1.6.4.4").revisions).default;
        "SHA".flags.exe = false;
        "split".revision = (((hackage."split")."0.2.3.4").revisions).default;
        "x509-validation".revision = (((hackage."x509-validation")."1.6.11").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.1").revisions).default;
        "extensible-exceptions".revision = (((hackage."extensible-exceptions")."0.1.1.4").revisions).default;
        };
      compiler = {
        version = "8.10.5";
        nix-name = "ghc8105";
        packages = {
          "binary" = "0.8.8.0";
          "ghc-prim" = "0.6.1";
          "exceptions" = "0.10.4";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          "template-haskell" = "2.16.0.0";
          "pretty" = "1.1.3.6";
          "process" = "1.6.9.0";
          "base" = "4.14.2.0";
          "rts" = "1.0.1";
          "text" = "1.2.4.1";
          "mtl" = "2.2.2";
          "time" = "1.9.3";
          "unix" = "2.7.2.2";
          "bytestring" = "0.10.12.0";
          "containers" = "0.6.4.1";
          "directory" = "1.3.6.0";
          "parsec" = "3.1.14.0";
          "ghc-boot-th" = "8.10.5";
          "filepath" = "1.4.2.1";
          "deepseq" = "1.4.4.0";
          "transformers" = "0.5.6.2";
          "Cabal" = "3.2.1.0";
          "stm" = "2.5.0.1";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        yuntan-device = ./.plan.nix/yuntan-device.nix;
        haxl = ./.plan.nix/haxl.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "yuntan-device" = { flags = {}; };
          "haxl" = { flags = { "bench" = lib.mkOverride 900 false; }; };
          };
        })
    ];
  }