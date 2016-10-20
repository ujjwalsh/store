# store

The 'store' package provides efficient binary serialization. There are a couple
features that particularly distinguish it from most prior Haskell serialization
libraries:

* Its primary goal is speed. By default, direct machine representations are used
  for things like numeric values (`Int`, `Double`, `Word32`, etc) and buffers
  (`Text`, `ByteString`, `Vector`, etc). This means that much of serialization
  uses the equivalent of `memcpy`.

  We have plans for supporting architecture independent serialization - see
  [#36](https://github.com/fpco/store/issues/36) and
  [#31](https://github.com/fpco/store/issues/31). This plan makes little endian
  the default, so that the most common endianness has no overhead.

* Instead of implementing lazy serialization / deserialization involving
  multiple input / output buffers, `peek` an `poke` always work with a single
  buffer. This buffer is allocated by asking the value for its size before
  encoding. This simplifies the encoding logic, and allows for highly optimized
  tight loops.

* `store` can optimize size computations by knowing when some types always
  use the same number of bytes.  This allows us to compute the byte size of a
  `Vector Int32` by just doing `length v * 4`.

It also features:

* Optimized serialization instances for many types from base, vector,
  bytestring, text, containers, time, template-haskell, and more.

* TH and GHC Generics based generation of Store instances for datatypes

* TH generation of testcases.

## Blog posts

* [Initial release announcement](https://www.fpcomplete.com/blog/2016/05/store-package)
* [Benchmarks of the prototype](https://www.fpcomplete.com/blog/2016/03/efficient-binary-serialization)
* [New 'weigh' allocation benchmark package](https://www.fpcomplete.com/blog/2016/05/weigh-package),
  created particularly to aid optimizing `store`.
