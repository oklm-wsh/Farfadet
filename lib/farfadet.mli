(** Farfadet module. *)

type 'a t = Faraday.t -> 'a -> unit
(** A {i writer}. It's a function which takes the Faraday's encoder, a value
   ['a] and write the value in the encoder. *)

val beint16 : int    t
(** Writer of a 16-bits integer in big-endian order. *)
val beint32 : int32  t
(** Writer of a 32-bits integer in big-endian order. *)
val beint64 : int64  t
(** Writer of a 64-bits integer in big-endian order. *)
val leint16 : int    t
(** Writer of a 16-bits integer in little-endian order. *)
val leint32 : int32  t
(** Writer of a 32-bits integer in little-endian order. *)
val leint64 : int64  t
(** Writer of a 64-bits integer in little-endian order. *)

val string     : string t
(** Writer of a [string]. *)
val bytes      : Bytes.t t
(** Writer of a [Bytes.t]. *)
val bigstring  : Faraday.bigstring t
(** Writer of a [Faraday.bigstring]. *)

val bool    : bool   t
(** Writer of a [bool]. If the value is [true], the writer writes ['1'],
    otherwise, it writes ['0']. *)
val char    : char   t
(** Writer of a [char]. *)

val seq     : 'a t -> 'b t -> ('a * 'b) t
(** [seq a b] composes 2 writers to one. It writes [a], then [b]. *)
val option  : 'a t -> 'a option t
(** [option a] makes a new writer and expects an option value. If the valus is
    [None], nothing happens. Otherwise, we call the writer [a]. *)
val list    : ?sep:('a t * 'a) -> 'b t -> 'b list t
(** [list ~sep a] makes a new writer and expects a list of value (which have the
    type [a]). [list ~sep a] writes each value with the writer [a] interspersed
    by the writer [sep]. *)

val comap   : 'a t -> ('b -> 'a) -> 'b t
(** [comap a f] makes a writer which runs [f] with the value and pass its
    results to [a]. *)

(** Operations on slices *)

type 'a sub = Faraday.t -> ?off:int -> ?len:int -> 'a -> unit
(** A sub-{i writer} with optional arguments [off] and [len] to specify which
    slice need to be written. *)

val substring    : string sub
(** Writer of a sub-[string]. *)
val subbytes     : bytes sub
(** Writer of a sub-[Bytes.t]. *)
val subbigstring : Faraday.bigstring sub
(** Writer of a sub-[Faraday.bigstring]. *)
val blitter      : ('a -> int) -> ('a -> src_off:int -> Faraday.bigstring -> dst_off:int -> len:int -> unit) -> 'a sub
(** [blitter len blit] makes a new sub-writer which uses the specific [blit]
    function to {i blit} the value to the internal [Faraday.bigstring] and the
    specific [length] function to get the length of the value ['a] (used when
    the optional argument [len = None]).

    eg. If you want to use a [memcpy] implementation instead a standalone
    [memmove] implementation. *)

val whole : 'a sub -> 'a t
(** Erases the optional arguments [off] and [len] from a {!sub} writer by [None]. *)

type vec = { off : int option ; len : int option }
(** A record which contains the optional values [off] and [len]. *)
val sub : 'a sub -> (vec * 'a) t
(** [sub a] makes a new writer from a {!sub}-writer which expect a {!vec} to
    specify the optional arguments [off] and [len]. *)

(** Formatters *)

type ('ty, 'v) order
(** Representation of the action applied to the Faraday's encoder. *)

type ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | (::) :
       ('ty, 'v) order
     * ( 'v, 'r) fmt
    -> ('ty, 'r) fmt

val atom    : 'a t -> ('a -> 'v, 'v) order
(** [atom writer] makes a new {!order} to apply the [writer] to the Faraday's
    encoder. *)
val subatom : 'a sub -> (vec -> 'a -> 'v, 'v) order
(** [subatom subwriter] makes a new {!order} to apply the [subwriter] to the
    Faraday's encoder. *)
val (!!)    : 'a t -> ('a -> 'v, 'v) order
(** Prefix operator of {!atom}. *)
val (!^)    : 'a sub -> (vec -> 'a -> 'v, 'v) order
(** Prefix operator of {!subatom}. *)

val concat : ('ty, 'v) fmt -> ( 'v, 'r) fmt -> ('ty, 'r) fmt
(** [concat fmt1 fmt2] concats [fmt1] with [fmt2]. *)
val yield : ('v, 'v) order
(** [yield] orders a [Faraday.yield] operations to the Faraday's encoder. *)
val flush : (unit -> unit) -> ('v, 'v) order
(** [flush f] registers [f] to be called when all prior writes have been
    successfully completed to the Faraday's encoder. *)

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

val const : 'a t -> 'a -> ('v, 'v) order
(** [const kind value] calls the writer [kind] with the value [value] directly. *)
val ($)   : 'a t -> 'a -> ('v, 'v) order
(** Infix operator of {!const}. *)

(** Conveniences operations *)

module Const :
sig
  val char      : char -> ('a,'a) order
  (** Write directly a character. *)
  val string    : ?off:int -> ?len:int -> string -> ('a,'a) order
  (** Write directly a [string]. *)
  val bytes     : ?off:int -> ?len:int -> Bytes.t -> ('a,'a) order
  (** Write directly a [Bytes.t]. *)
  val bigstring : ?off:int -> ?len:int -> Faraday.bigstring -> ('a,'a) order
  (** Write directly a [Faraday.bigstring]. *)
end

module Sched :
sig
  val bigstring : (vec -> Faraday.bigstring -> 'a, 'a) order
  (** Schedule a [Faraday.bigstring]. *)
end
