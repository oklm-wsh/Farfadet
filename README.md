# Farfadet, a printf-like for [Faraday](https://github.com/inhabitedtype/faraday) library

To use this library, you can pin it with `faraday`:
```
opam pin add faraday https://github.com/inhabitedtyped/faraday.git
opam pin add farfadet https://github.com/oklm-wsh/Farfadet.git
```

## Quick look

Firstly, you need to understand what
is [Faraday](https://github.com/inhabitedtype/faraday). Then, you can serialize
something like a this data:

```ocaml
type t = 
  [ `Bool of bool
  | `Float of float
  | `Null
  | `String of string
  | `A of t list
  | `O of (string * t) list ]
```

You can write something with Faraday, like that:

```ocaml
let rec write_json enc = function
  | `Bool true -> Faraday.write_string enc "true"
  | `Bool false -> Faraday.write_string enc "false"
  ...
  | `A lst ->
    Faraday.write_char enc '[';
    
    let rec aux = function
      | [] -> ()
      | [ x ] -> write_json x
      | x :: r -> write_json x; Faraday.write_char ','; aux r
    in 
    
    aux lst;
    Faraday.write_char enc ']'
```

And it's boring ... Yes. So, `Farfadet` can help you to write a serializer in a
type safe way. This is an example:

```ocaml
let comma =
  let open Farfadet in 
  (fun e () -> string e ","), ()
  
let rec value : t Farfadet.t = fun e x ->
  let open Farfadet in 
  
  let arr = list ~sep:comma value in
  
  match x with
  | `Bool true -> string e "true"
  | `Bool false -> string e "false"
  ...
  | `A lst -> eval e [ char $ '['; !!arr; char $ ']'] lst
```

It's much better. And it's like a `printf` function in OCaml with a little
overhead to facilite the serialization of any data with a `Faraday` backend. And
you can do more.

Another example is to use a `memcpy` implementation instead a `memmove`
implementation (provided by the standard library).

In fact, you can create your *blitter* and use it inside [Faraday] like:

```ocaml
let memcpy s soff d doff len =
  for i = 0 to len - 1
  do Bigarray.Array1.set dst (doff + i) (String.get s (soff + i)) done

let string' : string Farfadet.t = fun e -> eval e [ !!(whole @@ blitter String.length memcpy) ]
```

You can see the documentation to understand this snippet. A good example is
provided in the test to serialize a [Ezjsonm.t] value.

## Build Requirements

 * Faraday (dev version)
 * OCaml (>= 4.03.0)
 * A MirageOS hackathon

## Feedback

It's a Proof of Concept and you can improve the library like you want!
