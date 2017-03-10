
type 'a t = Faraday.t -> 'a -> unit

val beint16 : int    t
val beint32 : int32  t
val beint64 : int64  t
val leint16 : int    t
val leint32 : int32  t
val leint64 : int64  t

val string  : string t
val bytes   : Bytes.t t
val bigstring  : Faraday.bigstring t

val bool    : bool   t
val char    : char   t

val seq     : 'a t -> 'b t -> ('a * 'b) t
val option  : 'a t -> 'a option t
val list    : ?sep:('a t * 'a) -> 'b t -> 'b list t

val comap   : 'a t -> ('b -> 'a) -> 'b t

(** Operations on slices *)

type 'a sub = Faraday.t -> ?off:int -> ?len:int -> 'a -> unit

val substring : string sub
val subbytes : bytes sub
val subbigstring : Faraday.bigstring sub

val whole : 'a sub -> 'a t

type vec = { off : int option ; len : int option }
val sub : 'a sub -> (vec * 'a) t

(** Formatters *)

type ('ty, 'v) order
type ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | (::) :
       ('ty, 'v) order
     * ( 'v, 'r) fmt
    -> ('ty, 'r) fmt

val atom : 'a t -> ('a -> 'v, 'v) order
val subatom : 'a sub -> (vec -> 'a -> 'v, 'v) order
val (!!) : 'a t -> ('a -> 'v, 'v) order
val (!^) : 'a sub -> (vec -> 'a -> 'v, 'v) order

val concat : ('ty, 'v) fmt -> ( 'v, 'r) fmt -> ('ty, 'r) fmt
val yield : ('v, 'v) order
val flush : (unit -> unit) -> ('v, 'v) order

val keval :
     Faraday.t
  -> ('ty, 'v) fmt
  -> (Faraday.t -> 'v)
  -> 'ty

val eval  :
     Faraday.t
  -> ('ty, unit) fmt
  -> 'ty

(** Constants *)

val const : 'a t -> 'a -> ('a, 'a) order
val ($) : 'a t -> 'a -> ('a, 'a) order

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
