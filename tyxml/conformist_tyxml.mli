module Make (H : Html_sigs.T) : sig
type ('e, 'attr, 'kind, 'meta, 'ty) field

type input_attr = Html_types.input_attrib H.attrib
type select_attr = Html_types.select_attrib H.attrib

val custom :
  ('attr list -> 'e) ->
  'kind ->
  ('meta, 'a) Conformist.Field.t ->
  ('e, 'attr, 'kind, 'meta, 'a) field

val text_input :
  ('a -> string) ->
  Html_types.input_type ->
  ?default:'a ->
  ('meta, 'a) Conformist.Field.t ->
  ([> Html_types.input ] H.elt, input_attr, [> `Required], 'meta, 'a) field

type choice = {
  value: string;
  label: string;
}

val map_choice : ('a -> string) -> 'a -> choice

type ('e, 'attr, 'kind, 'item, 'sugg, 'meta, 'a) complex =
  (string -> ('item, Conformist.error_msg) result) ->
  ('item -> choice) ->
  'sugg list ->
  ?default:'a ->
  ?type_:string ->
  ?meta:'meta ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, 'attr, 'kind, 'meta, 'a) field

val radio :
  ?dir:[`LTR | `RTL] ->
  ([> Html_types.label ] H.elt list, input_attr, [> `Required],
   'a, 'a, 'meta, 'a) complex

val select_one :
  ([> Html_types.select ] H.elt, select_attr, [> `Required],
   'a, string * 'a list, 'meta, 'a) complex

val select_list :
  ([> Html_types.select ] H.elt, select_attr, [> `Many],
   'a, string * 'a list, 'meta, 'a list) complex

val bool :
  ?default:bool ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  string ->
  ([> Html_types.input] H.elt, input_attr, [> `Bool], 'meta, bool) field

val optional :
  ?meta:'meta ->
  ('e, 'attr, [`Required], 'meta, 'ty) field ->
  ('e, 'attr, [> `Optional], 'meta, 'ty option) field

type ('e, 'kind, 'meta, 'a) simple =
  ?default:'a ->
  ?meta:'meta ->
  ?msg:Conformist.error_msg ->
  ?validator:'a Conformist.validator ->
  string ->
  ('e, input_attr, 'kind, 'meta, 'a) field

val string_or_empty :
  ([> Html_types.input] H.elt, [> `String_or_empty], _, string) simple

val float : ([> Html_types.input] H.elt, [> `Required], _, float) simple
val int : ([> Html_types.input] H.elt, [> `Required], _, int) simple
val string : ([> Html_types.input] H.elt, [> `Required], _, string) simple
val datetime : ([> Html_types.input] H.elt, [> `Required], _, Ptime.t) simple

val render :
  ?attr:([> `Required | `Name] H.attrib as 'attr) list ->
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
end
