{
  lib,
  config,
  dream2nix,
  ...
}: {
  imports = [
    dream2nix.modules.dream2nix.nodejs-package-lock-v3
    dream2nix.modules.dream2nix.nodejs-granular-v3
  ];

  mkDerivation = {
    src = ./.;
    version = "0.1";
  };

  deps = {nixpkgs, ...}: {
    inherit
      (nixpkgs)
      stdenv
      solc
      ;
  };

  nodejs-package-lock-v3 = {
    packageLockFile = "${config.mkDerivation.src}/package-lock.json";
  };

  name = "zeko-da-layer";
  version = "0.1";

  paths.projectRoot = ./.;
  paths.package = ./.;
  paths.projectRootFile = "dream2nix-module.nix";

  env.SOLC_COMPILER_PATH = "${config.deps.solc}/bin/solc";
  env.SOLC_COMPILER_VERSION = config.deps.solc.version;
}
