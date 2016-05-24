# store [![Build Status](https://travis-ci.org/fpco/store.png)](https://travis-ci.org/fpco/store)

The 'store' package provides binary serialization of Haskell datatypes. It fills
quite a different niche from packages like 'binary' or 'cereal'. In particular:

* Its primary goal is speed. Whenever possible, direct machine representations
  are used. For numeric types (`Int`, `Double`, `Word32`, etc) and types that
  use buffers (`Text`, `ByteString`, `Vector`, etc).  This means that much of
  serialization uses the equivalent of `memcpy`.

* By using machine representations, we lose serialization compatibility between
  different architectures. Store could in theory be used to describe
  machine-independent serialization formats. However, this is not the usecase
  it's currently designed for (though utilities might be added for this in the
  future!)

* `Store` will not work at all on architectures which lack unaligned memory
  access (for example, older ARM processors).  This is not a fundamental
  limitation, but we do not currently require ARM support.

See
[this blog post](https://www.fpcomplete.com/blog/2016/03/efficient-binary-serialization)
which describes the initial motivations and benchmarks that led to the existence
of this package.
