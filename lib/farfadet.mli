module Blitter :
sig
  type 'a t =
    { blit   : 'a -> int -> Faraday.bigstring -> int -> int -> unit
    ; length : 'a -> int }
end

type ('ty, 'v) order

type ('ty, 'v) t =
  | [] : ('v, 'v) t
  | (::) :
       ('ty, 'v) order
     * ( 'v, 'r) t
    -> ('ty, 'r) t

type vec = { off : int
           ; len : int option }

module Const :
sig
  val char      : char -> ('a,'a) order
  val string    : ?off:int -> ?len:int -> string -> ('a,'a) order
  val bytes     : ?off:int -> ?len:int -> Bytes.t -> ('a,'a) order
  val bigstring : ?off:int -> ?len:int -> Faraday.bigstring -> ('a,'a) order
end

module Sched :
sig
  val string    : (vec -> String.t -> 'a, 'a) order
  val bytes     : (vec -> Bytes.t -> 'a, 'a) order
  val bigstring : (vec -> Faraday.bigstring -> 'a, 'a) order
end

type _ atom

val beint16 : int    atom
val beint32 : int32  atom
val beint64 : int64  atom
val leint16 : int    atom
val leint32 : int32  atom
val leint64 : int64  atom
val string  : string atom
val seq     : 'a atom -> 'b atom -> ('a * 'b) atom
val bool    : bool   atom
val char    : char   atom
val option  : 'a atom -> 'a option atom
(* val list    : 's const option -> 'a atom -> 'a list atom *)

val concat : ('ty, 'v) t -> ( 'v, 'r) t -> ('ty, 'r) t
val yield : ('v, 'v) order

val keval :
     Faraday.t
  -> ('ty, 'v) t
  -> (Faraday.t -> 'v)
  -> 'ty

val eval  :
     Faraday.t
  -> ('ty, unit) t
  -> 'ty
