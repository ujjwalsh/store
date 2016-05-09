* Take a look at the core generated for generics instances / TH instances.

* Document that we're not concerned with alignment issues. Have a test suite
  show that the base Store instances don't care about alignment on x86 / 64

* Why is Bool taking up 4 bytes?  Worth a custom instance?

* Storable instance in Base for Ratio throws errors when denominator is 0.
  Fixed via custom instance, but a patch to base should maybe be made.

* Add remaining unboxed vector instances (via TH)

* Benchmark Word vs Int for tags

* Based on speed of test, UTCTime seems to be slow

* Renamings:

  - Rename TaggedTH to something better? How about just Tagged?

  - Rename utilities like "pokeForeignPtr" to "pokeFromForeignPtr"
