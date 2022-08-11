type u = A | B | C [@@deriving show]
type t = {x: string; y: bool; z: u} [@@deriving show]

let u_of_string = function
  | "A" -> Ok A
  | "B" -> Ok B
  | "C" -> Ok C
  | _ -> Error "u_of_string"

let string_of_u = function
  | A -> "A"
  | B -> "B"
  | C -> "C"

open Conformist_tyxml

let direct =
  let open Tyxml.Html in
  let x, xs = render (string_or_empty ~meta:() "x") in
  let y, ys = render (bool "y" ~default:true) in
  let z, zs = render (select_one u_of_string string_of_u ["", [A;B;C]] "z") in
  form [
    div (txt "enter x: " :: x :: txt " and y: " :: y :: txt " then choose z: " :: [z]);
    button [txt "submit"];
  ],
  Conformist.(make Field.[xs; ys; zs]) (fun x y z -> {x; y; z})

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
  | Error (f, _, e) -> Printf.eprintf "%s: %s\n" f e
  | Ok t -> print_endline (show t)

let () = run direct
