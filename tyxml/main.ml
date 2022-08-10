type t = {x: string option; y: bool} [@@deriving show]

open Conformist_tyxml

let direct =
  let open Tyxml.Html in
  let x, xs = render (optional (string ~meta:() "x")) in
  let y, ys = render (bool "y" ~default:true) in
  div [txt "enter x: "; x; txt " and y: "; y],
  Conformist.(make Field.[xs; ys]) (fun x y -> {x; y})

(*
let monadic =
  let open Tyxml.Html in
  make (
    let< x = get (string ~meta:() "x") in
    let< y = get (bool "y" ~default:true) in
    return (div [txt "enter x: "; x; txt " and y: "; y])
  ) (fun x y -> {x; y})
*)

let run t =
  let f, sch = t in
  Format.printf "%a@." (Tyxml.Html.pp_elt ()) f;
  let params =
    Array.to_list Sys.argv |>
    List.tl |>
    List.map (fun x ->
      match String.split_on_char '=' x with
      | [k] -> k, []
      | [k; v] -> k, [v]
      | _ -> failwith "invalid format, expected key=value"
    )
  in
  match Conformist.decode sch params with
  | Error _ -> prerr_endline "error"
  | Ok t -> print_endline (show t)

let () = run direct
