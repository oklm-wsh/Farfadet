module Blitter =
struct
  type 'a t =
    { blit    : 'a -> int -> Faraday.bigstring -> int -> int -> unit
    ; length  : 'a -> int }
end

type vec = { off : int
           ; len : int option }

type (_, _) prelist =
  | Empty : ('a, 'a) prelist
  | Cons  :
        'a Blitter.t * ('h, 'r) prelist
    -> ('a Blitter.t -> 'h, 'r) prelist

type 'a const =
  | ConstChar      : char -> char const
  | ConstString    : vec * String.t -> String.t const
  | ConstBytes     : vec * Bytes.t -> Bytes.t const
  | ConstBigstring : vec * Faraday.bigstring -> Faraday.bigstring const

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

type 'a sched =
  | SchedString    : String.t sched
  | SchedBytes     : Bytes.t sched
  | SchedBigstring : Faraday.bigstring sched

module Schedule =
struct
  let string = SchedString
  let bytes = SchedBytes
  let bigstring = SchedBigstring
end

module Const =
struct
  let char s =
    ConstChar s

  let string    ?(off = 0) ?len s =
    ConstString    ({ off; len }, s)
  let bytes     ?(off = 0) ?len s =
    ConstBytes     ({ off; len }, s)
  let bigstring ?(off = 0) ?len s =
    ConstBigstring ({ off; len }, s)
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

type ('ty, 'v, 'final, 'cty, 'cv) ty =
  | Convertible :
       ('ty, 'v, 'cty, 'cv) list
    -> ('ty, 'v, [ `NotF ], 'cty, 'cv) ty
  | Finalized :
       ('ty, 'v, 'cty, ('ty, 'v, [ `F ], 'cv, 'cv) ty) list
    *           ('cty, ('ty, 'v, [ `F ], 'cv, 'cv) ty) prelist
    -> ('ty, 'v, [ `F ], 'cv, 'cv) ty

type ('ty, 'v, 'cty, 'cv) t =
  ('ty, 'v, 'cty, 'cv, 'cv) ty
  constraint 'cv = _ ty

let rec concat
  : type ty v r cty cv cr.
     (ty, v, cty, cv) list
  -> ( v, r,  cv, cr) list
  -> (ty, r, cty, cr) list
  = fun l1 l2 -> match l1 with
    | Nil -> l2
    | Close -> l2
    | Yield -> l2
    | Const   (x, r) -> Const   (x, concat r l2)
    | Direct  (x, r) -> Direct  (x, concat r l2)
    | Sched   (x, r) -> Sched   (x, concat r l2)
    | Blitter (x, r) -> Blitter (x, concat r l2)
    | Flush   (f, r) -> Flush   (f, concat r l2)
    | App     (f, r) -> App     (f, concat r l2)

let nil             = Nil
let close           = Close
let add_const   x r = Const   (x, r)
let add_direct  x r = Direct  (x, r)
let add_sched   x r = Sched   (x, r)
let add_blitter x r = Blitter (x, r)
let add_app     f r = App     (f, r)
let yield           = Yield
let flush f l       = Flush (f, l)

module Infix =
struct
  let nil             = Nil
  let close           = Close

  let ( ** )  x r = add_const x r
  let ( **! ) x r = add_direct x r
  let ( **~ ) x r = add_sched x r
  let ( **? ) x r = add_blitter x r
  let ( **| ) f l = flush f l
  let ( **= ) f r = add_app f r
  let ( @@ )      = concat
end

(* XXX(dinosaure): clash between {!ty} and [type ty']. *)

let rec finalize
  : type ty' v cty. (ty', v, _, cty, (ty', v, _, _) t) ty -> cty
  = fun (Convertible l) ->
    finalize_list l @@ fun ll -> Finalized (l, ll)
and finalize_list
  : type i ty v cty cv.
    (ty, v, cty, cv) list -> ((cty, cv) prelist -> cv) -> cty
  = fun p k -> match p with
    | Nil   -> k Empty
    | Close -> k Empty
    | Yield -> k Empty
    | Const      (_, r) -> finalize_list r k
    | Sched      (_, r) -> finalize_list r k
    | Direct     (_, r) -> finalize_list r k
    | Flush      (_, r) -> finalize_list r k
    | App        (_, r) -> finalize_list r k
    | Blitter    (_, r) ->
      fun c -> finalize_list r (fun cl -> k (Cons (c, cl)))

let write_const
  : type a. Faraday.t -> a const -> unit
  = fun encoder -> function
    | ConstString (v, s)    ->
      Faraday.write_string    encoder ~off:v.off ?len:v.len s
    | ConstBytes  (v, s)    ->
      Faraday.write_bytes     encoder ~off:v.off ?len:v.len s
    | ConstBigstring (v, s) ->
      Faraday.write_bigstring encoder ~off:v.off ?len:v.len s
    | ConstChar chr ->
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
      let rec aux = function
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

let sched_const
  : type a. Faraday.t -> a sched -> vec -> a -> unit
  = fun encoder -> function
    | SchedString ->
      fun v s -> Faraday.schedule_string    encoder ~off:v.off ?len:v.len s
    | SchedBytes ->
      fun v s -> Faraday.schedule_bytes     encoder ~off:v.off ?len:v.len s
    | SchedBigstring ->
      fun v s -> Faraday.schedule_bigstring encoder ~off:v.off ?len:v.len s

let rec eval_list
  : type ty v cty cv.
       Faraday.t
    -> (ty, v, cty, cv) list
    -> (cty, 'res) prelist
    -> ((cv, 'res) prelist -> Faraday.t -> v)
    -> ty
  = fun encoder l cl k -> match l with
    | Nil   ->
      k cl encoder
    | Close ->
      Faraday.close encoder
    | Yield ->
      Faraday.yield encoder;
      k cl encoder
    | Const (x, r) ->
      write_const encoder x;
      eval_list encoder r cl k
    | Sched (x, r) ->
      fun v s ->
        sched_const encoder x v s;
        eval_list encoder r cl k
    | Direct (a, r) ->
      fun x ->
        write_atom encoder a x;
        eval_list encoder r cl k
    | Blitter (a, r) ->
      let Cons (c, cl) = cl in
      fun vec x ->
        let off = match vec with Some { off; _ } -> Some off | None -> None in
        let len = match vec with Some { len; _ } -> len | None -> None in

        Faraday.write_gen
          encoder
          ~blit:c.Blitter.blit
          ~length:c.Blitter.length
          ?off ?len x;
        eval_list encoder r cl k
    | Flush (f, r) ->
      Faraday.flush encoder f;
      eval_list encoder r cl k
    | App   (f, r) ->
      fun x ->
        f encoder x;
        eval_list encoder r cl k

and keval_ty
  :    Faraday.t
    -> ('ty, 'v, 'cty, _ ty as 'res) list
    -> ('c, 'res) prelist
    -> (Faraday.t -> 'v) -> 'ty
  = fun encoder l cl k ->
    eval_list encoder l cl (fun Empty encoder -> k encoder)

let keval
  : type final. Faraday.t -> (_, _, final, _, _) ty -> _
  = fun encoder fmt k ->
    match fmt with
    | Finalized (fmt, cl) -> keval_ty encoder fmt cl k
    | Convertible fmt -> keval_ty encoder fmt Empty k

let eval encoder fmt = keval encoder fmt (fun x -> ())

let make l : _ ty =
  Convertible l
