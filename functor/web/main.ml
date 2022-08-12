open Js_of_ocaml
open Js_of_ocaml_tyxml

module H = struct
  include Tyxml_js.Html
  module Svg = Tyxml_js.Svg
end

module S = Shared.Make (H)

let main () =
  let f, _ = S.direct in
  let elt = Tyxml_js.To_dom.of_form f in
  let node = (elt :> Dom.node Js.t) in
  ignore (Dom_html.document##.body##appendChild node)


let _ =
  let f = Dom.handler (fun _ -> main (); Js._true) in
  ignore Dom_html.(addEventListener document Event.domContentLoaded f Js._true);
