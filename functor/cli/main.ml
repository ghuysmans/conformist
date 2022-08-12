open Shared

module H = struct
  include Tyxml.Html
  module Svg = Tyxml.Svg
end

module S = Shared.Make (H)

let () =
  let f, sch = S.direct in
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
