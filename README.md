# Farfadet, a printf-like for [Faraday](https://github.com/inhabitedtype/faraday) library

To use this library, you can pin it with `faraday`:
```
opam pin add faraday https://github.com/inhabitedtyped/faraday.git
opam pin add farfadet https://github.com/oklm-wsh/Farfadet.git
```

## Quick look

Firstly,        you        need        to        understand        what       is
[Faraday](https://github.com/inhabitedtype/faraday).  Then,  you  can  serialize
something like a this data:

```ocaml
  type tz_offset =
    { sign    : [ `Plus | `Minus ]
    ; hours   : int
    ; minutes : int }
  type user =
    { name  : string
    ; email : string
    ; date  : int64 * tz_offset option }
```

You can write something with Faraday, like that:

```ocaml
let write_user enc user =
  Faraday.write_string enc user.name;
  Faraday.write_char enc ' ';
  Faraday.write_string enc user.email;
  Faraday.write_char enc ' ';

  ...

  Faraday.yield enc
```

And it's boring ...  Yes. So, `Farfadet` can help you to write a serializer in a
type safe way. This is an example:

```ocaml
open Farfadet
open Farfadet.Infix

let write_user enc user =
  let fmt = string **! sp ** string **! ... yield in
  let fmt = finalize (make user) in

  eval enc fmt user.name user.email ...
```

It's much  better.  And it's  like a `printf`  function in  OCaml with  a little
overhead to  facilite the serialization of  any data  with a  `Faraday` backend.
And you can do more.

Another  example  is  to  use  a  `memcpy`  implementation  instead  a `memmove`
implementation (provided by  the  standard  library)  and  it's  why  we need to
[finalize] an [fmt].

In fact, you can create your *blitter* and use it inside [Faraday] like:

```ocaml
(* a memcpy blitter for string *)
let my_blitter = Blitter.{ blit = fun src src_off dst dst_off len -> ...
                         ; length = String.length }

let write_string_with_memcpy enc str =
  let fmt = string **? yield in
  let fmt = finalize (make fmt) my_blitter in

  eval enc fmt None str
```

The [None] argument can  be  used  to  specified  a  [vec]  value and specify an
offset and a  length of what you  want to `blit` (so we  repercute theses values
in [src_off] and [len] as parameter of your `blit` function).

## Build Requirements

 * Faraday (dev version)
 * OCaml (>= 4.02.0)
 * A MirageOS hackathon

## Feedback

It's a Proof of Concept and you can improve the library like you want!
