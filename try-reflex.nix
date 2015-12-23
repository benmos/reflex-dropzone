sysPkgs: import (sysPkgs.fetchgit {
    url    = git://github.com/ryantrinkle/try-reflex;
    # ghcjs-improved-base-2:
    rev    = "9d2fa99b8e8630dc73e34f96eb13709606a9705f";
    sha256 = "f7dce6b1c177dedc68438a90cdc142049a20ebc901f329e962b40f94b8c457b4";
    # old-base:
    # rev    = "70a7230df219d604e25caf22c22c7a5553c30af4";
    # sha256 = "b0b95ff5d578230578d2a42163c8577a1aaac71528da30bea075ade4fce548fb";
  }) {}
