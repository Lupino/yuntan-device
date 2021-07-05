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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "yuntan-device"; version = "0.1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Li Meng Jun";
      homepage = "https://github.com/Lupino/yuntan-user#readme";
      url = "";
      synopsis = "Initial project template from stack";
      description = "Please see README.md";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "config.sample.yaml" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."scotty" or (errorHandler.buildDepError "scotty"))
          (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."psql-utils" or (errorHandler.buildDepError "psql-utils"))
          (hsPkgs."aeson-result" or (errorHandler.buildDepError "aeson-result"))
          (hsPkgs."scotty-utils" or (errorHandler.buildDepError "scotty-utils"))
          (hsPkgs."scotty-haxl" or (errorHandler.buildDepError "scotty-haxl"))
          (hsPkgs."aeson-helper" or (errorHandler.buildDepError "aeson-helper"))
          (hsPkgs."rediscaching-haxl" or (errorHandler.buildDepError "rediscaching-haxl"))
          (hsPkgs."hedis" or (errorHandler.buildDepError "hedis"))
          (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."cache" or (errorHandler.buildDepError "cache"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
          (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          ];
        buildable = true;
        modules = [
          "Device"
          "Device/API"
          "Device/RawAPI"
          "Device/Types"
          "Device/DataSource"
          "Device/DataSource/Device"
          "Device/DataSource/Table"
          "Device/MQTT"
          "Device/Handler"
          "Device/Config"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "yuntan-device" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."yuntan-device" or (errorHandler.buildDepError "yuntan-device"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."scotty" or (errorHandler.buildDepError "scotty"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."psql-utils" or (errorHandler.buildDepError "psql-utils"))
            (hsPkgs."scotty-haxl" or (errorHandler.buildDepError "scotty-haxl"))
            (hsPkgs."rediscaching-haxl" or (errorHandler.buildDepError "rediscaching-haxl"))
            (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }