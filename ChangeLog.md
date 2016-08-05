# ChangeLog

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
