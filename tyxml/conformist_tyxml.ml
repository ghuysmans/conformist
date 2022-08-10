type ('e, 'attr, 'meta, 'ty) field = {
  render: 'attr list -> 'e;
  field: ('meta, 'ty) Conformist.Field.t;
}

type input_attr = Html_types.input_attrib Tyxml.Html.attrib

let text_input to_string input_type ?prefill field =
  let render attr =
    let open Tyxml.Html in
    let a =
      (a_input_type input_type :: attr) |> fun l ->
      match prefill with
      | None ->  l
      | Some x -> a_value (to_string x) :: l
    in
    input ~a ()
  in
  {render; field}

let optional ?meta {render; field} =
  {render; field = Conformist.optional ~empty:true ?meta field}

type ('e, 'meta, 'a) simple =
  ?prefill:'a ->
  ?default:'a ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, input_attr, 'meta, 'a) field

let bool ?default ?meta ?msg name =
  let render attr =
    let open Tyxml.Html in
    let a =
      a_input_type `Checkbox ::
      a_value "true" ::
      match default with
      | Some true -> a_checked () :: attr
      | _ -> attr
    in
    input ~a ()
  in
  {render; field = Conformist.bool ~default:false ?meta ?msg name}

let float ?prefill ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (float ?default ?meta ?msg ?validator name)) |>
  text_input string_of_float `Number ?prefill

let int ?prefill ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (int ?default ?meta ?msg ?validator name)) |>
  text_input string_of_int `Number ?prefill

let string ?prefill ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (string ?default ?meta ?msg ?validator name)) |>
  text_input (fun x -> x) `Text ?prefill

let datetime ?prefill ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (datetime ?default ?meta ?msg ?validator name)) |>
  text_input Ptime.to_rfc3339 `Datetime ?prefill

let render ?(attr=[]) {render; field} =
  let attr =
    if Conformist.Field.(is_optional (AnyField field)) then
      attr
    else
      Tyxml.Html.a_required () :: attr
  in
  render attr, field

(*
type ('a, 'meta, 'ctor, 'ty) t

let return x = fun st -> st, x

let (let<) : type a meta ctor ty ty2.
    (a, meta, ctor, ty) t ->
    (a -> ('b, meta, ty, ty2) t) ->
    ('b, meta, ctor, ty2) t = fun x f st ->
  let st', y = x st in
  f y st'

let make t f =
  let list, body = t Conformist.Field.[] in
  body, Conformist.make list f
*)