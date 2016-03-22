* Tests

* Benchmark generics instance vs TH

* Benchmark whether throwing exceptions is faster than plumbing 'Either String'

* Have 'deriveManyStoreFromStorable' take both a whitelist and blacklist. This
  will allow the TH to notify the user when there's some new 'Store' type that
  could have an instance. This allows them to make a conscious decision about
  it.

* Mention that we're not concerned with alignment issues.  Have a test suite
  show that the base Store instances don't care about alignment on x86 / 64

* Sanity check length values peeked (and throw errors)

* Export utilities for types based on bytearray / foreignptr / ptr / etc
