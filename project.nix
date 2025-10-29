{ config, nix-haskell-patches, lib, pkgs, ... }:

let nix-thunk = config.importing.nix-thunk;
    deps = with nix-thunk; mapSubdirectories thunkSource ./deps;
    # nixpkgs = import deps.nixpkgs {};
    obelisk = (import deps.obelisk{}).command;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
  ];

  source-repository-packages = {
    "higher-kinded" = deps.higher-kinded + "/higher-kinded";
    "higher-kinded-data" = deps.higher-kinded + "/higher-kinded-data";
    "higher-kinded-types" = deps.higher-kinded + "/higher-kinded-types";

    "reflex-dom-attrs" = deps.reflex-dom-attrs;
    "reflex-dom-tables" = deps.reflex-dom-tables;
  };

  name = "reflex-dom-lazy";
  src = ./.;
  compiler-nix-name = "ghc912";

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs ];
    packages = ps: with ps; [
      reflex-dom-lazy
    ];
    withHaddock = true;
    withHoogle = true;
    tools = {
      haskell-language-server = {};
    };

    # The TMP thing is necessary for vscode devcontainer, which starts up a nix-shell, sets the env variable in that
    # There's probably a way to do it without that
    # The optional string bit is cut verbatim from hnix: https://github.com/input-output-hk/haskell.nix/blob/e34dc3262c41fee2eb1e076475705e3e5e4b1450/modules/shell.nix#L67
    # I'm not sure if pkgs will get the correct version, actually
    shellHook = lib.mkForce (
      ''
        export TMPDIR=/tmp
        export TMP=/tmp
        export TEMP=/tmp
        EM_CACHE=$(mktemp -d -t emcache-ghcjs-XXXXXX)
      ''
      # + lib.optionalString
      #     pkgs.stdenv.hostPlatform.isGhcjs
          
      #     ''
      #   if [ -z "$EM_CACHE" ]; then
      #     # Create a unique temporary directory using mktemp
      #     EM_CACHE_DIR=$(mktemp -d -t emcache-ghcjs-XXXXXX)

      #     # Copy the default Emscripten cache contents to the temporary directory
      #     DEFAULT_EM_CACHE="${pkgs.pkgsBuildBuild.emscripten}/share/emscripten/cache"
      #     if [ -d "$DEFAULT_EM_CACHE" ]; then
      #       cp -r "$DEFAULT_EM_CACHE"/* "$EM_CACHE_DIR" 2>/dev/null || true
      #       chmod -R u+w "$EM_CACHE_DIR"
      #     fi

      #     export EM_CACHE="$EM_CACHE_DIR"
      #     echo "Set EM_CACHE to $EM_CACHE"
      #   else
      #     echo "EM_CACHE already set to $EM_CACHE"
      #   fi
      # ''
    );
    #TODO: Move this elsewhere?
    inputsFrom = [(pkgs.mkShell {
      packages = [
        pkgs.nil
        pkgs.direnv
        pkgs.nix-direnv
        obelisk
        pkgs.github-cli
      ];
    })];
  };
}
