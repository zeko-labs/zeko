# Node.js GraphQL API over the zeko archive database, built from a pinned
# zeko-labs/archive-node-api revision. The recipe mirrors the NixOS overlay in
# the machines repo (machines/overlays/archive-node-api.nix).
#
# nixpkgs' `buildNpmPackage` is deliberately not used: the nix-npm-buildPackage
# overlay applied in flake.nix shadows that attribute with serokell's unrelated
# builder. This uses the nixpkgs npm hooks that `buildNpmPackage` wraps.
{ lib, stdenv, fetchFromGitHub, fetchNpmDeps, npmHooks, nodejs }:
stdenv.mkDerivation (finalAttrs: {
  pname = "archive-node-api";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "zeko-labs";
    repo = "archive-node-api";
    rev = "a568868cd68e319f31ee55c84506af68d4225147";
    hash = "sha256-Q0sMwMcw4guyIuC8ObK16KAM9I0XwUqpVJVYACCQ+Cg=";
  };

  npmDeps = fetchNpmDeps {
    name = "${finalAttrs.pname}-${finalAttrs.version}-npm-deps";
    inherit (finalAttrs) src;
    hash = "sha256-SYKyvwPMZgt/ssB8z0J2vydGQE/LN+zYoa/3BOtv7hQ=";
  };

  nativeBuildInputs = [
    nodejs
    nodejs.python
    npmHooks.npmConfigHook
    npmHooks.npmBuildHook
  ];
  buildInputs = [ nodejs ];
  strictDeps = true;
  dontStrip = true;

  # package.json: "build": "npm run clean && npx tsc" (outDir ./build).
  npmBuildScript = "build";
  # The artillery devDependency pulls in playwright, which must not try to
  # download browsers during `npm ci`.
  env.PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";

  # schema.graphql is read relative to the working directory at run time
  # (src/resolvers.ts), so the runtime files are kept together under lib/.
  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib
    cp -r build node_modules package.json package-lock.json schema.graphql $out/lib
    runHook postInstall
  '';

  passthru = { inherit nodejs; };

  meta = {
    description = "An archive node graphql API";
    homepage = "https://github.com/zeko-labs/archive-node-api";
    platforms = nodejs.meta.platforms;
  };
})
