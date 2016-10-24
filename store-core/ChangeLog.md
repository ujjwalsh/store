# ChangeLog

## 0.3

* Adds support for alignment sensitive architectures, by using temporary buffers
  when necessary. This required changing the type of both Poke and Peek. Most
  user code should be unaffected, but this is still a breaking change.

## 0.2.0.1

* Fixes a bug that could result in segfaults when reading corrupted data.

## 0.2.0.0

* First public release
