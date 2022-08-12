type ('e, 'attr, 'kind, 'meta, 'ty) field = {
  render: 'attr list -> 'e;
  field: ('meta, 'ty) Conformist.Field.t;
  kind: 'kind;
}

type input_attr = Html_types.input_attrib Tyxml.Html.attrib
type select_attr = Html_types.select_attrib Tyxml.Html.attrib

let custom render kind field = {render; field; kind}

let text_input to_string input_type ?default field =
  let render attr =
    let open Tyxml.Html in
    let a =
      (a_input_type input_type :: attr) |> fun l ->
      match default with
      | None ->  l
      | Some x -> a_value (to_string x) :: l
    in
    input ~a ()
  in
  {render; field; kind = `Required}

let optional ?meta {render; field; kind = `Required} =
  {render; field = Conformist.optional ~empty:true ?meta field; kind = `Optional}

let decode_choice of_string = function
  | [] | [""] -> Error "No value provided"
  | [x] -> of_string x
  | _ -> Error "Too many values provided"

type ('e, 'attr, 'kind, 'item, 'sugg, 'meta, 'a) complex =
  (string -> ('item, Conformist.error_msg) result) ->
  ('item -> string) ->
  'sugg list ->
  ?default:'a ->
  ?type_:string ->
  ?meta:'meta ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, 'attr, 'kind, 'meta, 'a) field

let radio of_string to_string l ?default ?type_ ?meta ?validator name =
  let render attr =
    let open Tyxml.Html in
    List.map (fun x ->
      let a =
        a_input_type `Radio ::
        a_value (to_string x) ::
        match default with
        | Some y when x = y -> a_checked () :: attr
        | _ -> attr
      in
      input ~a ()
    ) l
  in
  let dec = decode_choice of_string in
  let enc x = [to_string x] in
  let field = Conformist.custom dec enc ?type_ ?meta ?validator name in
  {render; field; kind = `Required}

let render_select to_string default groups attr =
  let open Tyxml.Html in
  let options l =
    option ~a:[a_value ""] (txt "") ::
    List.map (fun x ->
      let a =
        if List.mem x default then
          [a_selected ()]
        else
          []
      in
      option ~a (txt (to_string x))
    ) l
  in
  select ~a:attr @@
  match groups with
  | ["", l] -> options l
  | _ -> List.map (fun (label, l) -> optgroup ~label (options l)) groups

let select_one of_string to_string groups ?default ?type_ ?meta ?validator name =
  let dec = decode_choice of_string in
  let enc x = [to_string x] in
  let field = Conformist.custom dec enc ?type_ ?meta ?validator name in
  let render = render_select to_string (Option.to_list default) groups in
  {render; field; kind = `Required}

let select_list of_string to_string groups ?(default=[]) ?type_ ?meta ?validator name =
  let dec l =
    List.fold_right (fun x t ->
      if x = "" then
        t
      else
        Result.bind (of_string x) (fun x ->
          Result.bind t (fun t -> Ok (x :: t))
        )
    ) l (Ok [])
  in
  let enc = List.map to_string in
  let field = Conformist.custom dec enc ?type_ ?meta ?validator name in
  let render attr =
    render_select to_string default groups
      (Tyxml.Html.a_multiple () :: attr)
  in
  {render; field; kind = `Many}

type ('e, 'kind, 'meta, 'a) simple =
  ?default:'a ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, input_attr, 'kind, 'meta, 'a) field

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
  {render; field = Conformist.bool ~default:false ?meta ?msg name; kind = `Bool}

let float ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (float ?meta ?msg ?validator name)) |>
  text_input string_of_float `Number ?default

let int ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (int ?meta ?msg ?validator name)) |>
  text_input string_of_int `Number ?default

let string ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (string ?meta ?msg ?validator name)) |>
  text_input (fun x -> x) `Text ?default

let string_or_empty ?default ?meta ?msg ?validator name =
  let t =
    Conformist.(string ?meta ?msg ?validator name) |>
    text_input (fun x -> x) `Text ?default
  in
  {t with kind = `String_or_empty}

let datetime ?default ?meta ?msg ?validator name =
  Conformist.(required_in_form (datetime ?meta ?msg ?validator name)) |>
  text_input Ptime.to_rfc3339 `Datetime ?default

let render ?(attr=[]) {render; field; kind} =
  let open Tyxml.Html in
  let attr =
    match kind with
    | `Required -> a_required () :: attr
    | _ -> attr
  in
  let attr = a_name (Conformist.Field.(name (AnyField field))) :: attr in
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
