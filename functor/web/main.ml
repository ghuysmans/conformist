open Shared
open Js_of_ocaml
open Js_of_ocaml_tyxml

module H = struct
  include Tyxml_js.Html
  module Svg = Tyxml_js.Svg
end

module S = Shared.Make (H)

let main () =
  let l, sch = S.direct in
  let f = H.form l in
  let elt = Tyxml_js.To_dom.of_form f in
  let node = (elt :> Dom.node Js.t) in
  ignore (Dom_html.document##.body##appendChild node);
  let h =
    Dom.handler (fun _ ->
      Printf.printf "submit\n";
      let params =
        Form.get_form_contents elt |>
        List.map (fun (x, y) ->
          Printf.printf "%s=%S\n" x y;
          x, [y]
        )
      in
      match Conformist.decode sch params with
      | Error (f, _, e) ->
        (* this shouldn't happen *)
        Printf.eprintf "%s: %s\n" f e;
        Js._false
      | Ok t ->
        Dom_html.window##alert (Js.string (show t));
        Js._false
    )
  in
  ignore Dom_html.(addEventListener elt Event.submit h Js._true)


let _ =
  let f = Dom.handler (fun _ -> main (); Js._true) in
  ignore Dom_html.(addEventListener document Event.domContentLoaded f Js._true);
