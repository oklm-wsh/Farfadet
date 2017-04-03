type vec = { off : int option ; len : int option }

type 'a t = Faraday.t -> 'a -> unit
type 'a sub = Faraday.t -> ?off:int -> ?len:int -> 'a -> unit

type ('ty, 'v) order =
  | Yield     : ('v, 'v) order
  | Const     :
       'a t * 'a
    -> ('v, 'v) order
  | Atom      :
       'a t
    -> ('a -> 'v, 'v) order
  | SubAtom   :
       'a sub
    -> (vec -> 'a -> 'v, 'v) order
  | Flush     :
       (unit -> unit)
    -> ('v, 'v) order
  | Param     :
       ('a t -> 'a -> 'v, 'v) order

and ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | (::) :
       ('ty, 'v) order
     * ( 'v, 'r) fmt
    -> ('ty, 'r) fmt

(** Predefined farfadets *)

let int8    : _ t = Faraday.write_uint8
let beint16 : _ t = Faraday.BE.write_uint16
let beint32 : _ t = Faraday.BE.write_uint32
let beint64 : _ t = Faraday.BE.write_uint64
let leint16 : _ t = Faraday.LE.write_uint16
let leint32 : _ t = Faraday.LE.write_uint32
let leint64 : _ t = Faraday.LE.write_uint64
let char    : _ t = Faraday.write_char
let bool    : _ t = fun encoder -> function
  | true  -> char encoder '1'
  | false -> char encoder '0'

let substring           : _ sub = Faraday.write_string
let subbytes            : _ sub = Faraday.write_bytes
let subbigstring        : _ sub = Faraday.write_bigstring
let blitter length blit : _ sub = Faraday.write_gen ~length ~blit

let whole (a : _ sub) : _ t = a ?off:None ?len:None
let sub (a : _ sub)   : _ t = fun encoder ({off; len}, x) -> a ?off ?len encoder x

let string    = whole substring
let bytes     = whole subbytes
let bigstring = whole subbigstring

let seq f g : _ t = fun encoder (x,y) -> f encoder x; g encoder y

let list ?sep a : _ t =
  let sep = match sep with
    | None -> fun _ -> ()
    | Some (a, v) -> fun e -> a e v
  in
  let rec aux encoder : _ list -> _ = function
    | [] -> ()
    | [ x ] -> a encoder x
    | x :: r ->
      a encoder x;
      sep encoder;
      aux encoder r
  in
  aux

let option f : _ t =
  fun encoder -> function
    | Some a -> f encoder a
    | None   -> ()

let comap a f : _ t = fun encoder x -> a encoder (f x)

let atom f    = Atom f
let subatom f = SubAtom f
let (!!)      = atom
let (!^)      = subatom

(** Scheduled *)

module Sched = struct
  let string    = SubAtom Faraday.schedule_string
  let bytes     = SubAtom Faraday.schedule_bytes
  let bigstring = SubAtom Faraday.schedule_bigstring
end

(** Constants *)

let const a x          = Const (a,x)
let csub a ?off ?len x = Const ((fun e x -> a e ?off ?len x), x)
let ($)                = const

module Const = struct
  let char s                = const char s
  let string    ?off ?len x = csub substring ?off ?len x
  let bytes     ?off ?len x = csub subbytes ?off ?len x
  let bigstring ?off ?len x = csub subbigstring ?off ?len x
end

let yield   = Yield
let flush f = Flush f

let rec concat
  : type x v r. (x, v) fmt -> (v, r) fmt -> (x, r) fmt  = fun l1 l2 -> match l1 with
    | [] -> l2
    | x::r -> x :: concat r l2

let (^^) = concat

(** Output *)

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
    | Const (p, x) ->
      p encoder x;
      k encoder
    | SubAtom p ->
      fun v x ->
        sub p encoder (v, x) ;
        k encoder
    | Atom p ->
      fun x ->
        p encoder x;
        k encoder
    | Flush f ->
      Faraday.flush encoder f;
      k encoder
    | Param ->
      fun p x ->
        p encoder x;
        k encoder

let rec keval
  : type ty v cty cv.
       Faraday.t
    -> (ty, v) fmt-> (Faraday.t -> v)
    -> ty
  = fun encoder l k -> match l with
    | []   -> k encoder
    | o::l ->
      let k' encoder = keval encoder l k in
      keval_order encoder o k'

let eval encoder fmt = keval encoder fmt (fun x -> ())

let p e = eval e [ string $ "foo" ; Sched.bigstring ; !!beint32 ; yield ]
let p e = eval e [ string $ "foo" ]
