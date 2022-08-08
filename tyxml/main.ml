type t = {x: string; y: bool} [@@deriving show]

module Make (I : module type of Conformist_tyxml) = struct
  open I

  let direct =
    let open Tyxml.Html in
    let x, xs = string ~meta:() "x" in
    let y, ys = bool "y" ~default:true in
    div [txt "enter x: "; x; txt " and y: "; y],
    Conformist.(make Field.[xs; ys]) (fun x y -> {x; y})

  let monadic =
    let open Tyxml.Html in
    make (
      let< x = get (string ~meta:() "x") in
      let< y = get (bool "y" ~default:true) in
      return (div [txt "enter x: "; x; txt " and y: "; y])
    ) (fun x y -> {x; y})

  let run t =
    let f, sch = t in
    Format.printf "%a@." (Tyxml.Html.pp_elt ()) f;
    match Conformist.decode sch ["x", ["1"]; "y", []] with
    | Error _ -> prerr_endline "error"
    | Ok t -> print_endline (show t)
end
