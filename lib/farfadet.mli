module Blitter :
sig
  type 'a t =
    { blit   : 'a -> int -> Faraday.bigstring -> int -> int -> unit
    ; length : 'a -> int }
end

type vec =
  { off : int
  ; len : int option }

type 'a const =
  | ConstChar      : char -> char const

  | ConstString    : vec * String.t -> String.t const
  | ConstBytes     : vec * Bytes.t -> Bytes.t const
  | ConstBigstring : vec * Faraday.bigstring -> Faraday.bigstring const

type 'a sched =
  | SchedString    : String.t sched
  | SchedBytes     : Bytes.t sched
  | SchedBigstring : Faraday.bigstring sched

type _ atom =
  | BEu16  : int    atom
  | BEu32  : int32  atom
  | BEu64  : int64  atom
  | LEu16  : int    atom
  | LEu32  : int32  atom
  | LEu64  : int64  atom
  | String : string atom
  | Bool   : bool   atom
  | Char   : char   atom
  | Seq    : 'a atom * 'b atom -> ('a * 'b) atom
  | Opt    : 'a atom -> 'a option atom
  | List   : 'sep const option * 'a atom -> 'a list atom

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
val list    : 's const option -> 'a atom -> 'a list atom

module Const :
sig
  val char      : char -> char const

  val string    : ?off:int -> ?len:int -> string -> String.t const
  val bytes     : ?off:int -> ?len:int -> Bytes.t -> Bytes.t const
  val bigstring : ?off:int -> ?len:int -> Faraday.bigstring -> Faraday.bigstring const
end

module Schedule :
sig
  val string    : String.t sched
  val bytes     : Bytes.t sched
  val bigstring : Faraday.bigstring sched
end

type ('ty, 'v, 'cty, 'cv) list =
  | Nil       : ('v, 'v, 'cv, 'cv) list
  | Close     : (unit, unit, 'cv, 'cv) list
  | Yield     : ('v, 'v, 'cv, 'cv) list
  | Const     :
       'a const
    *  ('ty, 'v, 'cty, 'cv) list
    -> ('ty, 'v, 'cty, 'cv) list
  | Direct    :
       'a atom
    *  ('ty,       'v, 'cty, 'cv) list
    -> ('a -> 'ty, 'v, 'cty, 'cv) list
  | Sched     :
       'a sched
    *  ('ty, 'v, 'cty, 'cv) list
    -> (vec -> 'a -> 'ty, 'v, 'cty, 'cv) list
  | Blitter   :
       'a atom
    *  ('ty,              'v,                 'cty, 'cv) list
    -> (vec option -> 'a -> 'ty, 'v, 'a Blitter.t -> 'cty, 'cv) list
  | Flush     :
       (unit -> unit)
    *  ('ty, 'v, 'cty, 'cv) list
    -> ('ty, 'v, 'cty, 'cv) list
  | App       :
       (Faraday.t -> 'a -> unit)
    *  ('ty, 'v, 'cty, 'cv) list
    -> ('a -> 'ty, 'v, 'cty, 'cv) list

type ('ty, 'v, 'final, 'cty, 'cv) ty

type ('ty, 'v, 'cty, 'cv) t =
  ('ty, 'v, 'cty, 'cv, 'cv) ty
  constraint 'cv = _ ty

val concat :
     ('ty, 'v, 'cty, 'cv) list
  -> ( 'v, 'r,  'cv, 'cr) list
  -> ('ty, 'r, 'cty, 'cr) list

val nil   : ('v, 'v, 'cv, 'cv) list
val close : (unit, unit, 'cv, 'cv) list
val yield : ('v, 'v, 'cv, 'cv) list

module Infix :
sig
  val ( ** )  :
       'a const
    -> ('ty, 'v, 'cty, 'cv) list
    -> ('ty, 'v, 'cty, 'cv) list
  val ( **! ) :
       'a atom
    -> ('ty, 'v, 'cty, 'cv) list
    -> ('a -> 'ty, 'v, 'cty, 'cv) list
  val ( **~ ) :
       'a sched
    -> ('ty, 'v, 'cty, 'cv) list
    -> (vec -> 'a -> 'ty, 'v, 'cty, 'cv) list
  val ( **? ) :
       'a atom
    -> ('ty, 'v, 'cty, 'cv) list
    -> (vec option -> 'a -> 'ty, 'v, 'a Blitter.t -> 'cty, 'cv) list
  val ( **| ) :
       (unit -> unit)
    -> ('ty, 'v, 'cty, 'cv) list
    -> ('ty, 'v, 'cty, 'cv) list
  val ( **= ) :
       (Faraday.t -> 'a -> unit)
    -> ('ty, 'v, 'cty, 'cv) list
    -> ('a -> 'ty, 'v, 'cty, 'cv) list
  val ( @@ )  :
       ('ty, 'v, 'cty, 'cv) list
    -> ( 'v, 'r,  'cv, 'cr) list
    -> ('ty, 'r, 'cty, 'cr) list
end

val add_const :
     'a const
  -> ('ty, 'v, 'cty, 'cv) list
  -> ('ty, 'v, 'cty, 'cv) list

val add_direct :
     'a atom
  -> ('ty, 'v, 'cty, 'cv) list
  -> ('a -> 'ty, 'v, 'cty, 'cv) list

val add_sched :
     'a sched
  -> ('ty, 'v, 'cty, 'cv) list
  -> (vec -> 'a -> 'ty, 'v, 'cty, 'cv) list

val add_blitter :
     'a atom
  -> ('ty, 'v, 'cty, 'cv) list
  -> (vec option -> 'a -> 'ty, 'v, 'a Blitter.t -> 'cty, 'cv) list

val flush :
     (unit -> unit)
  -> ('ty, 'v, 'cty, 'cv) list
  -> ('ty, 'v, 'cty, 'cv) list

val add_app     :
     (Faraday.t -> 'a -> unit)
  -> ('ty, 'v, 'cty, 'cv) list
  -> ('a -> 'ty, 'v, 'cty, 'cv) list

val finalize :
  ('ty, 'v, [ `NotF ], 'cty,
   ('ty, 'v, [ `F ], _) t) ty -> 'cty

val make     :
     ('ty, 'v, 'cty, 'cv) list
  -> ('ty, 'v, [ `NotF ], 'cty, 'cv) ty

val keval :
     Faraday.t
  -> ('ty, 'v, _, _) t
  -> (Faraday.t -> 'v)
  -> 'ty

val eval  :
     Faraday.t
  -> ('ty, unit, _, _) t
  -> 'ty
