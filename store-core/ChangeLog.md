# ChangeLog

## Unreleased

* Now builds with primitive >= 0.6.4.0

## 0.4.2

* Adds `unsafeMakePokeState`, `unsafeMakePeekState`, and
  `maybeAlignmentBufferSize`, so that library users can write their own
  `encode` / `decode` functions.
  See [#126](https://github.com/fpco/store/pull/126)

## 0.4.1

* Less aggressive inlining, resulting in faster compilation / simplifier
  not running out of ticks

## 0.4

* Changes result of Peek function to be strict.
  (See [#98](https://github.com/fpco/store/pull/98))

## 0.3

* Adds support for alignment sensitive architectures, by using temporary buffers
  when necessary. This required changing the type of both Poke and Peek. Most
  user code should be unaffected, but this is still a breaking change.

## 0.2.0.1

* Fixes a bug that could result in segfaults when reading corrupted data.

## 0.2.0.0

* First public release
