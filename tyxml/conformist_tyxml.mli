type ('e, 'attr, 'kind, 'meta, 'ty) field

type input_attr = Html_types.input_attrib Tyxml.Html.attrib

val custom :
  ('attr list -> 'e) ->
  'kind ->
  ('meta, 'a) Conformist.Field.t ->
  ('e, 'attr, 'kind, 'meta, 'a) field

val text_input :
  ('a -> string) ->
  Html_types.input_type ->
  ?prefill:'a ->
  ('meta, 'a) Conformist.Field.t ->
  ([> Html_types.input ] Tyxml.Html.elt, input_attr, [> `Required], 'meta, 'a) field

val radio :
  (string -> ('a, Conformist.error_msg) result) ->
  ('a -> string) ->
  'a list ->
  ?default:'a ->
  ?type_:string ->
  ?meta:'meta ->
  ?validator:'a Conformist.validator ->
  string ->
  ([> Html_types.input ] Tyxml.Html.elt list, input_attr, [> `Required], 'meta, 'a) field

val select_one :
  (string -> ('a, Conformist.error_msg) result) ->
  ('a -> string) ->
  (string * 'a list) list ->
  ?default:'a ->
  ?type_:string ->
  ?meta:'meta ->
  ?validator:'a Conformist.validator ->
  string ->
  ([> Html_types.select ] Tyxml.Html.elt, Html_types.select_attrib Tyxml.Html.attrib, [> `Required], 'meta, 'a) field

val select_list :
  (string -> ('a, Conformist.error_msg) result) ->
  ('a -> string) ->
  (string * 'a list) list ->
  ?default:'a list ->
  ?type_:string ->
  ?meta:'meta ->
  ?validator:'a list Conformist.validator ->
  string ->
  ([> Html_types.select ] Tyxml.Html.elt, Html_types.select_attrib Tyxml.Html.attrib, [> `Many], 'meta, 'a list) field

val bool :
  ?default:bool ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  string ->
  ([> Html_types.input] Tyxml.Html.elt, input_attr, [> `Bool], 'meta, bool) field

val string_or_empty :
  ?prefill:string ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  ?validator:string Conformist.validator ->
  string ->
  ([> Html_types.input] Tyxml.Html.elt, input_attr, [> `String_or_empty], 'meta, string) field

val optional :
  ?meta:'meta ->
  ('e, 'attr, [`Required], 'meta, 'ty) field ->
  ('e, 'attr, [> `Optional], 'meta, 'ty option) field

type ('e, 'kind, 'meta, 'a) simple =
  ?prefill:'a ->
  ?default:'a ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, input_attr, 'kind, 'meta, 'a) field

val float : ([> Html_types.input] Tyxml.Html.elt, [> `Required], _, float) simple
val int : ([> Html_types.input] Tyxml.Html.elt, [> `Required], _, int) simple
val string : ([> Html_types.input] Tyxml.Html.elt, [> `Required], _, string) simple
val datetime : ([> Html_types.input] Tyxml.Html.elt, [> `Required], _, Ptime.t) simple

val render :
  ?attr:([> `Required | `Name] Tyxml.Html.attrib as 'attr) list ->
  ('e, 'attr, [> `Required], 'meta, 'ty) field ->
  'e * ('meta, 'ty) Conformist.Field.t

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
