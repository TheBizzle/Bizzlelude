Bizzlude
===============

A Prelude replacement.  Probably don't use it.

Turn on `NoImplicitPrelude` and `OverloadedStrings` if you want any reasonable chance of niceness while using it.

Better yet: Don't use it, since this is Yet Another Prelude Replacement®, untested and undocumented.

Also, it requires the the PCRE3 C library to be installed (as per the [instructions here](https://hackage.haskell.org/package/pcre-light)).  (I don't like this requirement, but I also don't like POSIX regexes, and the `regexpr` library seems to no longer be maintained.)
