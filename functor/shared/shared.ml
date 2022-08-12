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

module Make (H : Html_sigs.T) = struct
  open H

  let ret = H.Xml.W.return
  let txt x = txt (ret x)
  let cl l = H.Xml.W.(List.fold_right (fun x t -> cons (return x) t) l (nil ()))

  module C = Conformist_tyxml.Make (H)
  open C

  let direct =
    let x, xs = render (string_or_empty ~meta:() "x") in
    let y, ys = render (bool "y" ~default:true) in
    let z, zs = render (select_one u_of_string (map_choice string_of_u) ["", [A;B;C]] "z") in
    [
      div (cl (txt "enter x: " :: x :: txt " and y: " :: y :: txt " then choose z: " :: [z]));
      button (cl [txt "submit"]);
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
end
