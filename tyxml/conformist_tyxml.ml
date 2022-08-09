type ('e, 'meta, 'ty) field = 'e * ('meta, 'ty) Conformist.Field.t

let prepend o l =
  match o with
  | None ->  l
  | Some a -> a :: l

let prepend_const b x l =
  if b then x :: l else l

let text_input to_string input_type attr ?prefill field =
  let open Tyxml.Html in
  let a =
    (a_input_type input_type :: attr) |>
    prepend (Option.map (fun x -> a_value (to_string x)) prefill) |>
    prepend_const (not Conformist.Field.(is_optional (AnyField field))) (a_required ())
  in
  input ~a (), field

type ('e, 'meta, 'a) simple =
  ?prefill:'a ->
  ?default:'a ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, 'meta, 'a) field

let bool ?default ?meta ?msg name =
  Tyxml.Html.(input ~a:(
    a_input_type `Checkbox ::
    a_value "true" ::
    match default with
    | Some true -> [a_checked ()]
    | _ -> []
  ) ()),
  Conformist.bool ~default:false ?meta ?msg name

let float ?prefill ?default ?meta ?msg ?validator name =
  Conformist.float ?default ?meta ?msg ?validator name |>
  text_input string_of_float `Number [] ?prefill

let int ?prefill ?default ?meta ?msg ?validator name =
  Conformist.int ?default ?meta ?msg ?validator name |>
  text_input string_of_int `Number [] ?prefill

let string ?prefill ?default ?meta ?msg ?validator name =
  Conformist.string ?default ?meta ?msg ?validator name |>
  text_input (fun x -> x) `Text [] ?prefill

let datetime ?prefill ?default ?meta ?msg ?validator name =
  Conformist.datetime ?default ?meta ?msg ?validator name |>
  text_input Ptime.to_rfc3339 `Datetime [] ?prefill

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
