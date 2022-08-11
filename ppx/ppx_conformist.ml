open Ppxlib
open Ast_helper

let append ~suffix txt =
  match txt, suffix with
  | "t", _ -> suffix
  | _, "" -> txt
  | _ -> txt ^ "_" ^ suffix

let name_attr =
  Attribute.(declare "conformist.name" Context.constructor_declaration)
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let mk_pat ~suffix txt loc =
  Pat.var {txt = append ~suffix txt; loc}

let mk_id ~suffix txt loc =
  Exp.ident ~loc {txt = lident (append ~suffix txt); loc}

let f_of_variant {txt; loc} l =
  match
    List.fold_right (fun ({pcd_name = {txt; loc}; pcd_args; _} as cd) t ->
      Result.bind t (fun t ->
        match pcd_args with
        | Pcstr_tuple [] ->
          let name =
            Attribute.get name_attr cd |>
            Option.value ~default:txt
          in
          let const = Const.string ~loc name in
          let lid = {txt = lident txt; loc} in
          let h =
            Exp.case
              [%pat? [ [%p Pat.constant ~loc const] ]]
              [%expr Result.Ok [%e Exp.construct ~loc lid None]],
            Exp.case
              (Pat.construct ~loc lid None)
              [%expr [ [%e Exp.constant ~loc const] ]]
          in
          Ok (h :: t)
        | _ ->
          Error Location.(
            loc,
            error_extensionf ~loc "ppx_conformist: expected an enum"
          )
      )
    ) l (Ok []) |>
    Result.map List.split
  with
  | Ok (dc, ec) ->
    let dc =
      let error = Exp.constant (Const.string ("Unknown " ^ txt ^ " provided")) in
      dc @ [Exp.case [%pat? _] [%expr Result.Error [%e error]]]
    in
    [%str
      let [%p mk_pat ~suffix:"decoder" txt loc] = [%e Exp.function_ ~loc dc]
      and [%p mk_pat ~suffix:"encoder" txt loc] = [%e Exp.function_ ~loc ec]]
  | Error (loc, e) ->
    [Str.extension ~loc e]

let key_attr =
  Attribute.(declare "conformist.key" Context.label_declaration)
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let field_of_type loc = function
  (* FIXME
  | [%type: [%t? typ] option] -> None, [%expr Conformist.optional [%e field_of_type loc typ]]
  | [%type: [%t? typ] list] -> None, [%expr Conformist.list [%e field_of_type loc typ]]
  *)
  | [%type: bool] -> None, [%expr Conformist.bool]
  | [%type: float] -> None, [%expr Conformist.float]
  | [%type: int] -> None, [%expr Conformist.int]
  | [%type: string] -> None, [%expr Conformist.string]
  | [%type: Ptime.t] -> None, [%expr Conformist.datetime]
  | {ptyp_desc = Ptyp_constr ({txt; loc}, _params); _} ->
    let name = Ppxlib_traverse.mangle_type_name txt in
    (*
    let f suffix =
      List.fold_left
        (fun acc typ -> [%expr [%e acc] [%e snd (field_of_type loc typ)]])
        (mk_id ~suffix name loc)
        params
    in
    *)
    Some name,
    [%expr
      Conformist.custom
        [%e mk_id ~suffix:"decoder" name loc]
        [%e mk_id ~suffix:"encoder" name loc]
        ~type_:[%e Exp.constant ~loc (Const.string ~loc name)]]
  | _ ->
    None,
    Location.error_extensionf ~loc "ppx_conformist: unsupported type" |>
    Exp.extension

let of_string_attr =
  Attribute.(declare "conformist.of_string" Context.label_declaration)
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let to_string_attr =
  Attribute.(declare "conformist.to_string" Context.label_declaration)
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let default_attr =
  Attribute.(declare "conformist.default" Context.label_declaration)
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let validator_attr =
  Attribute.(declare "conformist.validator" Context.label_declaration)
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let meta_attr =
  Attribute.(declare "conformist.meta" Context.label_declaration)
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let msg_attr =
  Attribute.(declare "conformist.msg" Context.label_declaration)
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let make_of_record loc l =
  let bindings =
    List.map (fun {pld_name = {txt; _}; _} ->
      let lid = {txt = Lident txt; loc} in
      lid, Exp.ident ~loc lid
    ) l
  in
  Exp.record ~loc bindings None |>
  List.fold_right (fun {pld_name = {txt; _}; _} body ->
    Exp.fun_ ~loc Nolabel None (Pat.var ~loc {txt; loc}) body
  ) l

let schema_of_record txt loc l =
  let fields =
    List.map (fun ({pld_name = {txt; loc}; pld_type; _} as ld) ->
      let key =
        let const =
          match Attribute.get key_attr ld with
          | None -> Const.string ~loc txt
          | Some x -> Const.string ~loc x
        in
        Exp.constant ~loc const
      in
      let f =
        match field_of_type loc pld_type with
        | Some name, f ->
          let msg_invalid =
            (* FIXME related to the optional argument? *)
            Exp.constant (Const.string ("Invalid " ^ txt ^ " provided"))
          in
          let msg_missing =
            Exp.constant (Const.string ("No value provided"))
          in
          let decoder e =
            match Attribute.get of_string_attr ld with
            | None -> e
            | Some f ->
              [%expr
                let [%p mk_pat ~suffix:"decoder" name loc] = function
                  | [x] ->
                    begin
                      try Result.Ok ([%e f] x)
                      with _ -> Result.Error [%e msg_invalid]
                    end
                  | _ -> Result.Error [%e msg_missing]
                in
                [%e e]]
          in
          let encoder e =
            match Attribute.get to_string_attr ld with
            | None -> e
            | Some f ->
              [%expr
                let [%p mk_pat ~suffix:"encoder" name loc] = fun x ->
                  [ [%e f] x ]
                in
                [%e e]]
          in
          encoder f |> decoder
        | None, f ->
          f
      in
      let app e attr =
        match Attribute.get attr ld with
        | None -> e
        | Some x ->
          match String.split_on_char '.' (Attribute.name attr) with
          | ["conformist"; name] -> Exp.apply ~loc e [Labelled name, x]
          | _ -> assert false
      in
      let a = [default_attr; validator_attr; meta_attr; msg_attr] in
      [%expr
        [%e List.fold_left app f a]
        [%e key]]
    ) l
  in
  let t =
    List.fold_right (fun f t -> [%expr Conformist.Field.(::) ([%e f], [%e t])])
      fields
      [%expr Conformist.Field.([])]
  in
  [%expr Conformist.make [%e t] [%e mk_id ~suffix:"" txt loc]]

let attributes = Attribute.[
  T name_attr;
  T key_attr;
  T of_string_attr;
  T to_string_attr;
  T default_attr;
  T validator_attr;
  T meta_attr;
  T msg_attr;
]

let generate_impl ~ctxt (_rec_flag, type_declarations) with_make =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match type_declarations with
  | [{ptype_name = {txt; _} as ptype_name; ptype_kind; ptype_manifest; _}] ->
    begin match ptype_kind, ptype_manifest with
    | Ptype_record fields, _ ->
      let sig_ =
        [%str
          let [%p mk_pat ~suffix:"schema" txt loc] =
            [%e schema_of_record txt loc fields]]
      in
      if with_make then
        [%stri
          let [%p mk_pat ~suffix:"" txt loc] =
            [%e make_of_record loc fields]] :: sig_
      else
        sig_
    | Ptype_variant cases, _ ->
      f_of_variant ptype_name cases
    | _ ->
      [Location.error_extensionf ~loc "ppx_conformist: unsupported kind" |>
       Str.extension]
    end
  | _ ->
    [Str.extension (Location.error_extensionf ~loc "ppx_conformist: unsupported")]

let generate_intf ~ctxt (_rec_flag, _type_declarations) =
  let _loc = Expansion_context.Deriver.derived_item_loc ctxt in
  failwith "TODO"


let impl_generator =
  let args = Deriving.Args.(empty +> flag "with_make") in
  Deriving.Generator.V2.make ~attributes ~deps:[] args generate_impl

let intf_generator =
  Deriving.Generator.V2.make_noarg ~attributes ~deps:[] generate_intf

let my_deriver =
  Deriving.add
    "conformist"
    ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
