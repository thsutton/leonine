Leonine
=======

[![Build status][travis-badge]][travis-link]

[*Leonine*][1] is an implementation of the [Roaring Bitmaps][2] data
structure for efficient representation of sets of 32-bit values.  For
more information about *leonine* see the [documentation][3] or refer
to the [Roaring Bitmaps][2] web-site for other implementations and
publications about the data structure.

**Please note:** This is a work in progress and is not yet ready for
use. When complete it will be released on Hackage.

Structure
---------

The *Roaring Bitmap* structure divides the 32-bit keys into two 16-bit
values. One, the high order bits, identifies a *chunk* within the
map and the other, the low order bits, identifies a *bit* within
the chunk.

There are two chunk representations:

1. A sparse chunk contains a `Word16` for each *bit* present in the
   chunk.

2. A dense chunk contains 4096 `Word16`s which contains exactly one
   bit for every possible *bit* which can be present in the chunk.

The structure will convert the representation of each chunk as *bit*s
are set and cleared from the map.

[1]: https://github.com/thsutton/leonine
[2]: http://www.roaringbitmaps.org/
[3]: https://hackage.haskell.org/package/leonine/docs/Data-Leonine.html

[travis-link]: https://travis-ci.org/thsutton/leonine
[travis-badge]: https://travis-ci.org/thsutton/leonine.svg?branch=master
