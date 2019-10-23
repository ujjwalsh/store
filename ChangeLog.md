# ChangeLog

## 0.6.0.1

* Now builds with GHC-7.10 - compatibility was broken in 0.6.0 due to
  the fix for GHC-8.8. See
  [#146][https://github.com/fpco/store/issues/146].

## 0.6.0

* Now builds with GHC-8.8. This is a major version bump because
  MonadFail constraints were added to some functions, which is
  potentially a breaking change.

## 0.5.1.2

* Fixes compilation with GHC < 8.0.  See
  [#142](https://github.com/fpco/store/issues/142).

## 0.5.1.1

* Update to the instances for generics, to improve error messages for
  sum types with more than 255 constructors.  See
  [#141](https://github.com/fpco/store/issues/141)

## 0.5.1.0

* Update to TH to support sum types with more than 62 constructors.

* Uses TH to derive Either instance, so that it can sometimes have ConstSize #119.

## 0.5.0.1

* Updates to test-suite enabling `store` to build with newer dependencies.

## 0.5.0

* `Data.Store.Streaming` moved to a separate package, `store-streaming`.

## 0.4.3.2

* Buildable with GHC 8.2

* Fix to haddock formatting of Data.Store.TH code example

## 0.4.3.1

* Fixed compilation on GHC 7.8

## 0.4.3

* Less aggressive inlining, resulting in faster compilation / simplifier
  not running out of ticks

## 0.4.2

* Fixed testsuite

## 0.4.1

* Breaking change in the encoding of Map / Set / IntMap / IntSet,
  to use ascending key order. Attempting to decode data written by
  prior versions of store (and vice versa) will almost always fail
  with a decent error message. If you're unlucky enough to have a
  collision in the data with a random Word32 magic number, then the
  error may not be so clear, or in extremely rare cases,
  successfully decode, yielding incorrect results. See
  [#97](https://github.com/fpco/store/issues/97) and
  [#101](https://github.com/fpco/store/pull/101).


* Performance improvement of the 'Peek' monad, by introducing more
  strictness.  This required a change to the internal API.

* API and behavior of 'Data.Store.Version' changed. Previously, it
  would check the version tag after decoding the contents. It now
  also stores a magic Word32 tag at the beginning, so that it fails
  more gracefully when decoding input that lacks encoded version
  info.

## 0.4.0

Deprecated in favor of 0.4.1

## 0.3.1

* Fix to derivation of primitive vectors, only relevant when built with
  primitive-0.6.2.0 or later

* Removes INLINE pragmas on the generic default methods.  This
  dramatically improves compilation time on recent GHC versions.
  See [#91](https://github.com/fpco/store/issues/91).

* Adds `instance Contravariant Size`

## 0.3

* Uses store-core-0.3.*, which has support for alignment sensitive
  architectures.

* Adds support for streaming decode from file descriptor, not supported on
  windows. As part of this addition, the API for "Data.Store.Streaming" has
  changed.

## 0.2.1.2

* Fixes a bug that could could result in attempting to malloc a negative
  number of bytes when reading corrupted data.

## 0.2.1.1

* Fixes a bug that could result in segfaults when reading corrupted data.

## 0.2.1.0

Release notes:

* Adds experimental `Data.Store.Version` and deprecates `Data.Store.TypeHash`.
  The new functionality is similar to TypeHash, but there are much fewer false
  positives of hashes changing.

Other enhancements:

* Now exports types related to generics

## 0.2.0.0

Release notes:

* Core functionality split into `store-core` package

Breaking changes:

* `combineSize'` renamed to `combineSizeWith`

* Streaming support now prefixes each Message with a magic number, intended to
  detect mis-alignment of data frames. This is worth the overhead, because
  otherwise serialization errors could be more catastrophic - interpretting some
  bytes as a length tag and attempting to consume many bytes from the source.

Other enhancements:

* [weigh](https://github.com/fpco/weigh) based allocations benchmark.

* Addition of `Array` / `UArray` instances

* Streaming support now has checks for over/undershooting buffer

Bug fixes:


## 0.1.0.0

* First public release
