module Blitter =
struct
  type 'a t =
    { blit    : 'a -> int -> Faraday.bigstring -> int -> int -> unit
    ; length  : 'a -> int }
end

type vec = { off : int
           ; len : int option }

type 'a const =
  | Char      : char -> char const
  | String    : vec * String.t -> String.t const
  | Bytes     : vec * Bytes.t -> Bytes.t const
  | Bigstring : vec * Faraday.bigstring -> Faraday.bigstring const

type 'a schedule =
  | String    : String.t schedule
  | Bytes     : Bytes.t schedule
  | Bigstring : Faraday.bigstring schedule

type _ atom =
  | BEu16   : int    atom
  | BEu32   : int32  atom
  | BEu64   : int64  atom
  | LEu16   : int    atom
  | LEu32   : int32  atom
  | LEu64   : int64  atom
  | String  : string atom
  | Bool    : bool   atom
  | Char    : char   atom
  | Seq     : 'a atom * 'b atom -> ('a * 'b) atom
  | Opt     : 'a atom -> 'a option atom
  | List    : 'sep const option * 'a atom -> 'a list atom

type ('ty, 'v) order =
  | Yield     : ('v, 'v) order
  | Const     :
       'a const
    -> ('v, 'v) order
  | Direct    :
       'a atom
    -> ('a -> 'v, 'v) order
  | Sched     :
       'a schedule
    -> (vec -> 'a -> 'v, 'v) order
  | Blitter   :
       'a atom * 'a Blitter.t
    -> (vec option -> 'a -> 'v, 'v) order
  | Flush     :
       (unit -> unit)
    -> ('v, 'v) order
  | App       :
       (Faraday.t -> 'a -> unit)
    -> ('a -> 'v, 'v) order

type ('ty, 'v) t =
  | [] : ('v, 'v) t
  | (::) :
       ('ty, 'v) order
     * ( 'v, 'r) t
    -> ('ty, 'r) t

let beint16       = BEu16
let beint32       = BEu32
let beint64       = BEu64
let leint16       = LEu16
let leint32       = LEu32
let leint64       = LEu64
let string        = String
let bool          = Bool
let char          = Char
let seq x y       = Seq (x, y)
let option x      = Opt x
let list sep x    = List (sep, x)

module Const = struct
  let char s = Const (Char s)
  let string    ?(off = 0) ?len s = Const (String ({ off; len }, s))
  let bytes     ?(off = 0) ?len s = Const (Bytes ({ off; len }, s))
  let bigstring ?(off = 0) ?len s = Const (Bigstring ({ off; len }, s))
end

module Sched = struct
  let string = Sched String
  let bytes = Sched Bytes
  let bigstring = Sched Bigstring
end

let yield = Yield

let rec concat
  : type x v r. (x, v) t -> (v, r) t -> (x, r) t
  = fun l1 l2 -> match l1 with
    | [] -> l2
    | x::r -> x :: concat r l2

let (^^) = concat

(** Output *)

let write_const
  : type a. Faraday.t -> a const -> unit
  = fun encoder -> function
    | String (v, s)    ->
      Faraday.write_string    encoder ~off:v.off ?len:v.len s
    | Bytes  (v, s)    ->
      Faraday.write_bytes     encoder ~off:v.off ?len:v.len s
    | Bigstring (v, s) ->
      Faraday.write_bigstring encoder ~off:v.off ?len:v.len s
    | Char chr ->
      Faraday.write_char      encoder chr

let rec write_atom
  : type a. Faraday.t -> a atom -> a -> unit
  = fun encoder -> function
    | BEu16 -> Faraday.BE.write_uint16 encoder
    | BEu32 -> Faraday.BE.write_uint32 encoder
    | BEu64 -> Faraday.BE.write_uint64 encoder
    | LEu16 -> Faraday.LE.write_uint16 encoder
    | LEu32 -> Faraday.LE.write_uint32 encoder
    | LEu64 -> Faraday.LE.write_uint64 encoder
    | Char  -> Faraday.write_char   encoder
    | String     ->
      fun x      -> Faraday.write_string encoder x
    | Seq (a, b) ->
      fun (x, y) ->
        write_atom encoder a x;
        write_atom encoder b y
    | Bool -> (function
               | true  -> Faraday.write_char encoder '1'
               | false -> Faraday.write_char encoder '0')
    | List (sep, a) ->
      let rec aux : _ list -> _ = function
        | [] -> ()
        | [ x ] -> write_atom encoder a x
        | x :: r ->
          write_atom encoder a x;
          (match sep with Some s -> write_const encoder s | None -> ());
          aux r
      in
      aux
    | Opt x      ->
      function Some a -> write_atom encoder x a
             | None   -> ()

let schedule_const
  : type a. Faraday.t -> a schedule -> vec -> a -> unit
  = fun encoder -> function
    | String ->
      fun v s -> Faraday.schedule_string    encoder ~off:v.off ?len:v.len s
    | Bytes ->
      fun v s -> Faraday.schedule_bytes     encoder ~off:v.off ?len:v.len s
    | Bigstring ->
      fun v s -> Faraday.schedule_bigstring encoder ~off:v.off ?len:v.len s

let keval_order
  : type ty v cty cv.
    Faraday.t
    -> (ty, v) order
    -> (Faraday.t -> v)
    -> ty
  = fun encoder o k -> match o with
    | Yield ->
      Faraday.yield encoder;
      k encoder
    | Const x ->
      write_const encoder x;
      k encoder
    | Sched x ->
      fun v s ->
        schedule_const encoder x v s;
        k encoder
    | Direct a ->
      fun x ->
        write_atom encoder a x;
        k encoder
    | Blitter (a, b) ->
      fun vec x ->
        let off = match vec with Some { off; _ } -> Some off | None -> None in
        let len = match vec with Some { len; _ } -> len | None -> None in
        Faraday.write_gen
          encoder
          ~blit:b.Blitter.blit
          ~length:b.Blitter.length
          ?off ?len x;
        k encoder
    | Flush f ->
      Faraday.flush encoder f;
      k encoder
    | App   f ->
      fun x ->
        f encoder x;
        k encoder

let rec keval
  : type ty v cty cv.
       Faraday.t
    -> (ty, v) t -> (Faraday.t -> v)
    -> ty
  = fun encoder l k -> match l with
    | []   -> k encoder
    | o::l ->
      let k' encoder = keval encoder l k in
      keval_order encoder o k'

let eval encoder fmt = keval encoder fmt (fun x -> ())
