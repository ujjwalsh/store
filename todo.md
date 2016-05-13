* Take a look at the core generated for generics instances / TH instances.

* Document that we're not concerned with alignment issues. Have a test suite
  show that the base Store instances don't care about alignment on x86 / 64

* Storable instance in Base for Ratio throws errors when denominator is 0.
  Fixed via custom instance, but a patch to base should maybe be made.

* Add remaining unboxed vector instances (via TH)

* Benchmark Word vs Int for tags

* Based on speed of test, UTCTime seems to be slow

* Something like Packer's holes?  What about holes for parsing too?

* Have an option to avoid copies and instead pin the input BS memory
