{ mkDerivation, stdenv,
  ghcjs-dom, reflex, reflex-dom, these
}:
mkDerivation {
  pname        = "reflex-dropzone";
  version      = "0.0";
  src          = builtins.filterSource (path: type:
                                        let base   = baseNameOf path;
                                            prefix = builtins.substring 0 1 base;
                                            suffix = builtins.substring (builtins.stringLength base - 1) 1 base;
                                        in
                                        prefix != "." && suffix != "#" && suffix != "~" &&        # Blacklist
                                        (stdenv.lib.hasPrefix (toString ./src) (toString path) || # Whitelist
                                         builtins.elem base ["reflex-dropzone.cabal" "Setup.hs" "LICENSE"]))
                                  ./.;
  buildDepends = [
    ghcjs-dom reflex reflex-dom these
  ];
  testDepends = [
  ];
  license = null;
}
