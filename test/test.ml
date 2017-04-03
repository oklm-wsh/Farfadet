let comma =
  let open Farfadet in
  (fun e () -> eval e [ string $ "," ]), ()

let memcpy s soff d doff len =
  for i = 0 to len - 1
  do Bigarray.Array1.set d (doff + i) (String.get s (soff + i)) done

let rec value : Ezjsonm.value Farfadet.t = fun e x ->
  let open Farfadet in

  let binding e (k, v) = eval e [ !!string; char $ ':'; !!value ] k v in
  let arr = list ~sep:comma value in
  let obj = list ~sep:comma binding in

  match x with
  | `Bool true  -> eval e [ string $ "true" ]
  | `Bool false -> eval e [ string $ "false" ]
  | `Float f    -> eval e [ string $ (Format.sprintf "%.16g" f) ]
  | `Null       -> eval e [ string $ "null" ]
  | `String s   -> eval e [ char $ '"'; !!(whole @@ blitter String.length memcpy); char $ '"' ] s (* for fun *)
  | `A a        -> eval e [ char $ '['; !!arr; char $ ']' ] a
  | `O o        -> eval e [ char $ '{'; !!obj; char $ '}' ] o

let json e x =
  let open Farfadet in

  let binding e (k, v) = eval e [ !!string; char $ ':'; !!value ] k v in
  let arr = list ~sep:comma value in
  let obj = list ~sep:comma binding in

  match x with
  | `A a -> eval e [ char $ '['; !!arr; char $ ']' ] a
  | `O o -> eval e [ char $ '{'; !!obj; char $ '}' ] o
  (* mais c'est trop beau ! *)

let test1 = `A [ `Bool true ]

let () =
  let e = Faraday.create 0x800 in

  Farfadet.(eval e [ !!json ] test1);
  Format.printf "%s\n%!" (Faraday.serialize_to_string e)
