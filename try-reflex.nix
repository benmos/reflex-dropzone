sysPkgs: import (sysPkgs.fetchgit {
    url    = git://github.com/ryantrinkle/try-reflex;
    # previous develop:
    # rev    = "70a7230df219d604e25caf22c22c7a5553c30af4";
    # sha256 = "b0b95ff5d578230578d2a42163c8577a1aaac71528da30bea075ade4fce548fb";
    # develop (inc ryan's latest GHCJS shims fix):
    # rev    = "bf29be22f546b5316862f95d881f38421cf9ed70";
    # sha256 = "be60e6c691743667013f27c17dc79fca0af0f970c449fa6b12be212305568b75";
    # previous ghcjs-improved-base-2:
    # rev    = "8854b2f46f4262d003bb2d6daf59d74d72772d53";
    # sha256 = "1177a27d19f276376b5b77a3b491642b1462e44e5cbd669b2e36a7a784da066f";
    # ghcjs-improved-base-2:
    rev    = "9d2fa99b8e8630dc73e34f96eb13709606a9705f";
    sha256 = "f7dce6b1c177dedc68438a90cdc142049a20ebc901f329e962b40f94b8c457b4";
  }) {}
