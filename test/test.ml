let comma =
  let open Farfadet in
  (fun e () -> string e ","), ()

let memcpy s ~src_off d ~dst_off ~len =
  for i = 0 to len - 1
  do Bigarray.Array1.set d (dst_off + i) (String.get s (src_off + i)) done

(* XXX(dinosaure): this code is WRONG. It's just a test and we don't care if we
                   respect the standard. Don't copy/paste. *)

let rec value : Ezjsonm.value Farfadet.t = fun e x ->
  let open Farfadet in

  let binding e (k, v) = eval e [ char $ '"'; !!string; char $ '"'; char $ ':'; !!value ] k v in
  let arr = list ~sep:comma value in
  let obj = list ~sep:comma binding in

  match x with
  | `Bool true  -> string e "true"
  | `Bool false -> string e "false"
  | `Float f    -> string e (Format.sprintf "%.16g" f)
  | `Null       -> string e "null"
  | `String s   -> eval e [ char $ '"'; !!(whole @@ blitter String.length memcpy); char $ '"' ] s (* just for fun *)
  | `A a        -> eval e [ char $ '['; !!arr; char $ ']' ] a
  | `O o        -> eval e [ char $ '{'; !!obj; char $ '}' ] o

let json e x =
  let open Farfadet in

  let binding e (k, v) = eval e [ char $ '"'; !!string; char $ '"'; char $ ':'; !!value ] k v in
  let arr = list ~sep:comma value in
  let obj = list ~sep:comma binding in

  match x with
  | `A a -> eval e [ char $ '['; !!arr; char $ ']' ] a
  | `O o -> eval e [ char $ '{'; !!obj; char $ '}' ] o

let tests =
  [ `A [ `Bool true ]
  ; `O [ ("a", `A [ `Bool true; `Bool false]) ]
  ; `A [ `O [ ("a", `Bool true); ("b", `Bool false) ] ] ]

let ezjsonm =
  Alcotest.testable
    (fun fmt value -> Format.fprintf fmt "%s" (Ezjsonm.to_string value))
    (=)

let make_test value =
  Format.sprintf "%s" (Ezjsonm.to_string value),
  `Slow,
  (fun () ->
     let enc = Faraday.create 0x800 in

     Farfadet.(eval enc [ !!json ] value);

     let str = Faraday.serialize_to_string enc in
     let res = Ezjsonm.from_string str in

     Alcotest.(check ezjsonm) str value res)

let () =
  Alcotest.run "farfadet test"
    [ "simple", List.map make_test tests ]
