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
    flags = { halvm = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "entropy"; version = "0.4.1.7"; };
      license = "BSD-3-Clause";
      copyright = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      maintainer = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      author = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      homepage = "https://github.com/TomMD/entropy";
      url = "";
      synopsis = "A platform independent entropy source";
      description = "A mostly platform independent method to obtain cryptographically strong entropy\n(RDRAND, urandom, CryptAPI, and patches welcome)\nUsers looking for cryptographically strong (number-theoretically\nsound) PRNGs should see the 'DRBG' package too.";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (errorHandler.setupDepError "filepath")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (errorHandler.setupDepError "directory")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.setupDepError "process")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (if compiler.isGhcjs && true || system.isGhcjs
          then [
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            ]
          else if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else (pkgs.lib).optional (!system.isHalvm) (hsPkgs."unix" or (errorHandler.buildDepError "unix")));
        libs = (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs)) ((pkgs.lib).optional (system.isWindows) (pkgs."advapi32" or (errorHandler.sysDepError "advapi32")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/entropy-0.4.1.7.tar.gz";
      sha256 = "a9063dfeb566b443e6ea101fbcc22f23d8cec8b9600bfd1378b0ecadf04be9ee";
      });
    }) // {
    package-description-override = "name:           entropy\r\n\r\nversion:        0.4.1.7\r\nx-revision: 2\r\ndescription:    A mostly platform independent method to obtain cryptographically strong entropy\r\n                (RDRAND, urandom, CryptAPI, and patches welcome)\r\n                Users looking for cryptographically strong (number-theoretically\r\n                sound) PRNGs should see the 'DRBG' package too.\r\nsynopsis:       A platform independent entropy source\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\ncopyright:      Thomas DuBuisson <thomas.dubuisson@gmail.com>\r\nauthor:         Thomas DuBuisson <thomas.dubuisson@gmail.com>\r\nmaintainer:     Thomas DuBuisson <thomas.dubuisson@gmail.com>\r\ncategory:       Data, Cryptography\r\nhomepage:       https://github.com/TomMD/entropy\r\nbug-reports:    https://github.com/TomMD/entropy/issues\r\nstability:      stable\r\n\r\n-- build-type:  Simple\r\n-- ^^ Used for HaLVM\r\nbuild-type:     Custom\r\n\r\n-- ^^ Test for RDRAND support using 'ghc'\r\ncabal-version:  >=1.10\r\ntested-with:    GHC == 8.2.2\r\n-- data-files:\r\nextra-source-files:   ./cbits/getrandom.c ./cbits/random_initialized.c ./cbits/rdrand.c, ./cbits/rdrand.h, README.md\r\n\r\n-- Notice to compile  with HaLVM the above 'build-type' must be changed\r\n-- to 'Simple' instead of 'Custom'.  The current build system naively\r\n-- runs GHC to determine if the compiler supports RDRAND before proceeding.\r\nflag halvm\r\n    description:        Build for the HaLVM\r\n    default:            False\r\n\r\n\r\ncustom-setup\r\n  setup-depends: Cabal >= 1.10 && < 3.9\r\n               , base < 5\r\n               , filepath < 1.5\r\n               , directory < 1.4\r\n               , process < 1.7\r\n\r\nlibrary\r\n  ghc-options:  -O2\r\n  exposed-modules: System.Entropy\r\n  if impl(ghcjs) || os(ghcjs)\r\n    other-modules: System.EntropyGhcjs\r\n  else {\r\n    if os(windows)\r\n      other-modules: System.EntropyWindows\r\n    else {\r\n         if os(halvm)\r\n           other-modules: System.EntropyXen\r\n         else\r\n           other-modules: System.EntropyNix\r\n    }\r\n  }\r\n  other-extensions:    CPP, ForeignFunctionInterface, BangPatterns,\r\n                       ScopedTypeVariables\r\n  build-depends:       base >= 4.8 && < 5, bytestring\r\n\r\n  default-language:    Haskell2010\r\n\r\n  if impl(ghcjs) || os(ghcjs) {\r\n    build-depends:     ghcjs-dom\r\n                     , jsaddle\r\n  }\r\n  else {\r\n    if(os(halvm))\r\n      cpp-options: -DXEN -DHAVE_RDRAND\r\n      cc-options:  -DXEN -DHAVE_RDRAND\r\n    if arch(x86_64)\r\n      cpp-options: -Darch_x86_64\r\n      cc-options:  -Darch_x86_64 -O2\r\n      -- gcc 4.8.2 on i386 fails to compile rdrand.c when using -fPIC!\r\n      c-sources:    cbits/rdrand.c\r\n      include-dirs: cbits\r\n    if arch(i386)\r\n      cpp-options: -Darch_i386\r\n      cc-options:  -Darch_i386 -O2\r\n    if os(windows)\r\n      build-depends: Win32 < 0\r\n      cpp-options: -DisWindows\r\n      cc-options:  -DisWindows\r\n      extra-libraries: advapi32\r\n    else\r\n      if !os(halvm)\r\n         Build-Depends: unix <2.8\r\n         c-sources: cbits/getrandom.c cbits/random_initialized.c\r\n  }\r\n\r\n\r\nsource-repository head\r\n    type:       git\r\n    location:   https://github.com/TomMD/entropy\r\n";
    }