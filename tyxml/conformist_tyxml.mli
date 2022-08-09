type ('e, 'meta, 'ty) field = 'e * ('meta, 'ty) Conformist.Field.t

(*
val custom :
  'a Conformist.decoder ->
  'a Conformist.encoder ->
  ('a option -> 'e) ->
  ?default:'a ->
  ?type_:string ->
  ?meta:'meta ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, 'meta, 'a) field
*)

val text_input :
  ('a -> string) ->
  Html_types.input_type ->
  Html_types.input_attrib Tyxml.Html.attrib list ->
  ?prefill:'a ->
  ('meta, 'a) Conformist.Field.t ->
  ([> Html_types.input ] Tyxml.Html.elt, 'meta, 'a) field

val bool :
  ?default:bool ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  string ->
  ([> Html_types.input] Tyxml.Html.elt, 'meta, bool) field

type ('e, 'meta, 'a) simple =
  ?prefill:'a ->
  ?default:'a ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, 'meta, 'a) field

val float : ([> Html_types.input] Tyxml.Html.elt, _, float) simple
val int : ([> Html_types.input] Tyxml.Html.elt, _, int) simple
val string : ([> Html_types.input] Tyxml.Html.elt, _, string) simple
val datetime : ([> Html_types.input] Tyxml.Html.elt, _, Ptime.t) simple

(*
type ('a, 'meta, 'ctor, 'ty) t

val return : 'a -> ('a, _, 'ty, 'ty) t
val get : ('e, 'meta, 'ty) field -> ('e, 'meta, 'ty -> 'tail, 'tail) t

val (let<) :
  ('a, 'meta, 'ctor, 'ty) t ->
  ('a -> ('b, 'meta, 'ty, 'ty2) t) ->
  ('b, 'meta, 'ctor, 'ty2) t

val make :
  ('e, 'meta, 'ctor, 'ty) t ->
  'ctor ->
  'e * ('meta, 'ctor, 'ty) Conformist.t
*)
