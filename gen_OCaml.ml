
open Printf
open Ptypes
open Gencode
open Camlp4
open PreCast
open Ast
open ExtList

type container = {
  c_name : string;
  c_types : Ast.str_item option;
  c_reader : Ast.str_item option;
  c_io_reader : Ast.str_item option;
  c_pretty_printer : Ast.str_item option;
  c_writer : Ast.str_item option;
}

let empty_container name ty_str_item =
  {
    c_name = name;
    c_types = Some ty_str_item;
    c_reader = None;
    c_io_reader = None;
    c_pretty_printer = None;
    c_writer = None
  }

let (|>) x f = f x
let (@@) f x = f x

let foldl1 msg f g = function
    [] -> invalid_arg ("foldl1: empty list -- " ^ msg)
  | hd::tl -> List.fold_left g (f hd) tl

let foldr1 msg f g = function
    [] -> invalid_arg ("foldr1: empty list -- " ^ msg)
  | l -> match List.rev l with
        [] -> assert false
      | hd::tl -> List.fold_left (fun s x -> g x s) (f hd) tl

let paSem_of_lidlist _loc l =
  Ast.paSem_of_list @@ List.map (fun s -> <:patt< $lid:s$>>) l

let exSem_of_lidlist _loc l =
  Ast.exSem_of_list @@ List.map (fun s -> <:expr< $lid:s$>>) l

let generate_container bindings =
  let _loc = Loc.mk "gen_OCaml" in

  let typedecl name ?(params = []) ctyp =
    Ast.TyDcl (_loc, name, params, ctyp, []) in

  let typedef name ?(params = []) ctyp =
      <:str_item< type $typedecl name ~params ctyp $ >> in

  let rec message_types msgname = function
      `Record l ->
        let ctyp (name, mutabl, texpr) =
          let ty = ctyp_of_texpr texpr in match mutabl with
              true -> <:ctyp< $lid:name$ : mutable $ty$ >>
            | false -> <:ctyp< $lid:name$ : $ty$ >> in
        let fields =
          foldl1 "message_types `Record" ctyp
            (fun ct field -> <:ctyp< $ct$; $ctyp field$ >>) l
        (* no quotations for type, wtf? *)
        (* in <:str_item< type $msgname$ = { $fields$ } >> *)
        in typedef msgname <:ctyp< { $fields$ } >>

   | `Sum l ->
       let tydef_of_msg_branch (const, mexpr) =
         message_types (msgname ^ "_" ^ const) (mexpr :> message_expr) in
       let record_types =
         foldl1 "message_types `Sum" tydef_of_msg_branch
           (fun s b -> <:str_item< $s$; $tydef_of_msg_branch b$ >>) l in

       let variant (const, _) =
         <:ctyp< $uid:const$ of ($lid: msgname ^ "_" ^ const $) >> in
       let consts = foldl1 "message_types `Sum" variant
                      (fun vars c -> <:ctyp< $vars$ | $variant c$ >>) l

       in <:str_item< $record_types$; $typedef msgname <:ctyp< [$consts$] >>$ >>

  and ctyp_of_texpr expr =
    type_expr expr |> reduce_to_poly_texpr_core bindings |> ctyp_of_poly_texpr_core

  and ctyp_of_poly_texpr_core = function
      `Bool -> <:ctyp< bool >>
    | `Byte -> <:ctyp< int >>
    | `Int _ -> <:ctyp< int >>
    | `Long_int -> <:ctyp< Int64.t >>
    | `Float -> <:ctyp< float >>
    | `String -> <:ctyp< string >>
    | `List ty -> <:ctyp< list $ctyp_of_poly_texpr_core ty$ >>
    | `Array ty -> <:ctyp< array $ctyp_of_poly_texpr_core ty$ >>
    | `Tuple l ->
        foldr1 "ctyp_of_poly_texpr_core `Tuple" ctyp_of_poly_texpr_core
          (fun ptexpr tup -> <:ctyp< ( $ ctyp_of_poly_texpr_core ptexpr $ * $tup$ ) >>)
          l
    | `Type (name, args) ->
        let t = List.fold_left (* apply *)
                  (fun ty ptexpr -> <:ctyp< $ty$ $ctyp_of_poly_texpr_core ptexpr$ >>)
                  <:ctyp< $uid:String.capitalize name$.$lid:name$ >>
                  args
        in (try <:ctyp< $id:Ast.ident_of_ctyp t$ >> with Invalid_argument _ -> t)
    | `Type_arg n -> <:ctyp< '$n$ >>

  in function
      Message_decl (msgname, mexpr) ->
        Some (empty_container msgname (message_types msgname mexpr))
    | Type_decl (name, params, texpr) ->
        let ty = match poly_beta_reduce_texpr bindings texpr with
            `Sum s -> begin
              let ty_of_const_texprs (const, ptexprs) =
                let tys = List.map ctyp_of_poly_texpr_core ptexprs in
                  <:ctyp< $uid:const$ of $Ast.tyAnd_of_list tys$>>

              in match s.constant with
                  [] -> foldl1 "generate_container Type_decl `Sum"
                          ty_of_const_texprs
                          (fun ctyp c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$ >>)
                          s.non_constant
                | _ ->
                    let const =
                      foldl1 "generate_container `Sum"
                        (fun tyn -> <:ctyp< $uid:tyn$ >>)
                        (fun ctyp tyn -> <:ctyp< $ctyp$ | $uid:tyn$ >>)
                        s.constant

                    in List.fold_left
                         (fun ctyp c -> <:ctyp< $ctyp$ | $ty_of_const_texprs c$ >>)
                         const s.non_constant
            end
          | #poly_type_expr_core ->
              reduce_to_poly_texpr_core bindings texpr |> ctyp_of_poly_texpr_core in
        let params =
          List.map (fun n -> <:ctyp< '$lid:type_param_name n$ >>) params
        in
          Some (empty_container name <:str_item< type $typedecl name ~params ty$ >>)

let loc = Camlp4.PreCast.Loc.mk

let maybe_str_item =
  let _loc = loc "<generated code>" in
    Option.default <:str_item< >>

module PrOCaml =Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax)

let string_of_ast f ast =
  let b = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer b in
  let o = new PrOCaml.printer () in
    Format.fprintf fmt "@[<v0>%a@]@." (f o) ast;
    Buffer.contents b

let generate_code containers =
  let _loc = loc "<generated code>" in
  let container_of_str_item c =
    <:str_item<
       module $String.capitalize c.c_name$ = struct
         $maybe_str_item c.c_types$;
         $maybe_str_item c.c_pretty_printer$;
         $maybe_str_item c.c_reader$;
         $maybe_str_item c.c_io_reader$;
         $maybe_str_item c.c_writer$
       end >>
  in string_of_ast (fun o -> o#implem)
       (List.fold_left
          (fun s c -> <:str_item< $s$; $container_of_str_item c$ >>)
          <:str_item< >>
          containers)

let list_mapi f l =
  let i = ref (-1) in
    List.map (fun x -> incr i; f !i x) l

let make_list f n = Array.to_list (Array.init f n)

module Pretty_print =
struct
  let _loc = Loc.mk "Gen_OCaml.Pretty_print"

  let expr_of_list l =
    List.fold_right (fun x l -> <:expr< [ $x$ :: $l$ ] >>) l <:expr< [] >>

  let pp_func name = <:expr< Extprot.Pretty_print.$lid:name$ >>

  let rec pp_message bindings msgname = function
      `Record l ->
        let pp_field (name, _, tyexpr) =
          <:expr<
            ( $str:String.capitalize msgname ^ "." ^ name$,
              $pp_func "pp_field"$ (fun t -> t.$lid:name$) $pp_texpr bindings tyexpr$ )
          >> in
        let pp_fields = List.map pp_field l in
          <:expr< $pp_func "pp_struct"$ $expr_of_list pp_fields$ pp >>
    | `Sum l ->
        let match_case (const, mexpr) =
          <:match_case<
            $uid:const$ t -> $pp_message bindings msgname (mexpr :> message_expr)$ t>>
        in <:expr< fun [ $Ast.mcOr_of_list (List.map match_case l)$ ] >>

  and pp_texpr bindings texpr =
    type_expr texpr |> reduce_to_poly_texpr_core bindings |> pp_poly_texpr_core

  and pp_poly_texpr_core = function
      `Bool -> pp_func "pp_bool"
    | `Byte -> pp_func "pp_int"
    | `Int _ -> pp_func "pp_int"
    | `Long_int -> pp_func "pp_int64"
    | `Float -> pp_func "pp_float"
    | `String -> pp_func "pp_string"
    | `List ty -> <:expr< $pp_func "pp_list"$ $pp_poly_texpr_core ty$ >>
    | `Array ty -> <:expr< $pp_func "pp_array"$ $pp_poly_texpr_core ty$ >>
    | `Tuple [ty] -> pp_poly_texpr_core ty
    | `Tuple l ->
        List.fold_left
          (fun e ptexpr -> <:expr< $e$ $pp_poly_texpr_core ptexpr$ >>)
          (pp_func ("pp_tuple" ^ string_of_int (List.length l)))
          l
    | `Type (name, args) ->
        List.fold_left
          (fun e ptexpr -> <:expr< $e$ $pp_poly_texpr_core ptexpr$ >>)
          <:expr< $uid:String.capitalize name$.$lid:"pp_" ^ name$ >>
          args
    | `Type_arg n -> <:expr< $lid:"pp_" ^ n$ >>

  let add_msgdecl_pretty_printer bindings msgname mexpr c =
    let expr = pp_message bindings msgname mexpr in
      { c with c_pretty_printer =
          Some <:str_item< value $lid:"pp_" ^ msgname$ pp = $expr$ >> }

  let add_typedecl_pretty_printer bindings tyname typarams texpr c =
    let wrap expr =
      List.fold_right
        (fun typaram e ->
           <:expr< fun $lid:"pp_" ^ type_param_name typaram$ -> $e$ >>)
        typarams
        expr in

    let expr = match poly_beta_reduce_texpr bindings texpr with
      `Sum s -> begin
        let constr_ptexprs_case (const, ptexprs) =
          let params = Array.to_list @@
                       Array.init (List.length ptexprs) (sprintf "v%d")
          in
            <:match_case<
              $uid:const$ $paSem_of_lidlist _loc params$ ->
                $pp_func "fprintf"$ pp
                $str:String.capitalize tyname ^ "." ^ const ^ " %a"$
                $pp_poly_texpr_core (`Tuple ptexprs)$ $exSem_of_lidlist _loc params$
            >> in
        let constr_case constr =
          <:match_case<
              $uid:constr$ -> $pp_func "fprintf"$ pp
                                $str:String.capitalize tyname ^ "." ^ constr$
          >> in
        let cases = List.concat
                      [
                        List.map constr_case s.constant;
                        List.map constr_ptexprs_case s.non_constant;
                      ]
        in <:expr< fun pp -> fun [ $Ast.mcOr_of_list cases$ ] >>
      end
      | #poly_type_expr_core ->
          reduce_to_poly_texpr_core bindings texpr |> pp_poly_texpr_core
    in
      { c with c_pretty_printer =
          Some <:str_item< value $lid:"pp_" ^ tyname$ = $wrap expr$ >> }

end


module Make_reader
         (RD : sig
            val reader_module : Ast.ident
            val reader_func : string -> string
          end) =
struct
  let read_field msgname constr_name name llty =
    let _loc = loc "<generated code @ field_match_cases>" in

    let rec read_elms f lltys_and_defs =
      (* TODO: handle missing elms *)
      let vars = List.rev @@ Array.to_list @@
                 Array.init (List.length lltys_and_defs) (sprintf "v%d")
      in
        List.fold_right
          (fun (n, llty, default) e ->
             let varname = sprintf "v%d" n in
               match default with
                   None ->
                     <:expr<
                       let $lid:varname$ =
                         if nelms >= $int:string_of_int (n+1)$ then
                           $read llty$
                         else
                           Extprot.Error.missing_tuple_element
                             ~message:$str:msgname$
                             ~constructor:$str:constr_name$
                             ~field:$str:name$
                             $int:string_of_int n$
                       in $e$
                     >>
                 | Some expr ->
                     <:expr<
                       let $lid:varname$ =
                         if nelms >= $int:string_of_int (n+1)$ then
                           $read llty$
                         else $expr$
                       in $e$
                     >>)
          (list_mapi (fun i (ty, default) -> (i, ty, default)) lltys_and_defs)
          (f (List.rev vars))

    and read_tuple_elms lltys_and_defs =
      let mk_tup vars =
        exCom_of_list @@ List.map (fun v -> <:expr< $lid:v$ >>) vars
      in read_elms mk_tup lltys_and_defs

    and read_sum_elms constr lltys =
      let c = constr in
      let mk_expr vars =
        List.fold_left
          (fun e var -> <:expr< $e$ $lid:var$ >>)
          <:expr< $uid:String.capitalize c.const_type$.$uid:c.const_name$ >>
          vars
      in read_elms mk_expr lltys

    and lltys_without_defaults = List.map (fun x -> (x, None))

    and read = function
        Vint Bool -> <:expr< $id:RD.reader_module$.read_bool s >>
      | Vint Int -> <:expr< $id:RD.reader_module$.read_rel_int s >>
      | Vint Positive_int -> <:expr< $id:RD.reader_module$.read_positive_int s>>
      | Bitstring32 -> <:expr< $id:RD.reader_module$.read_i32 s >>
      | Bitstring64 Long -> <:expr< $id:RD.reader_module$.read_i64 s >>
      | Bitstring64 Float -> <:expr< $id:RD.reader_module$.read_float s >>
      | Bytes -> <:expr< $id:RD.reader_module$.read_string s >>
      | Tuple lltys ->
          <:expr<
            let t = $id:RD.reader_module$.read_prefix s in
              match Extprot.Codec.ll_type t with [
                  Extprot.Codec.Tuple ->
                    let len = $id:RD.reader_module$.read_vint s in
                    let nelms = $id:RD.reader_module$.read_vint s in
                      $read_tuple_elms (lltys_without_defaults lltys)$
                | ll_type -> Extprot.Error.bad_wire_type ~ll_type ()
              ]
          >>
      | Sum (constant, non_constant) ->
          let constant_match_cases =
            List.map
              (fun c ->
                 <:match_case<
                   $int:string_of_int c.const_tag$ ->
                     $uid:String.capitalize c.const_type$.$lid:c.const_name$
                 >>)
              constant
            @ [ <:match_case<
                  tag -> Extprot.Error.unknown_tag tag >> ] in

          let nonconstant_match_cases =
            let mc (c, lltys) =
              <:match_case<
                 $int:string_of_int c.const_tag$ ->
                      $read_sum_elms c (lltys_without_defaults lltys)$
              >>
            in List.map mc non_constant @
               [ <:match_case<
                   tag -> Extprot.Error.unknown_tag tag >> ]
          in

          let maybe_match_case (constr, f, l) = match l with
              [] | [_] (* catch-all *)-> None
            | l ->
                let expr =
                  <:expr< match Extprot.Codec.ll_tag t with [ $Ast.mcOr_of_list l$ ] >>
                in
                  Some <:match_case< Extprot.Codec.$uid:constr$ -> $f expr$ >> in

          let wrap_non_constant e =
            <:expr<
              let len = $id:RD.reader_module$.read_vint s in
              let nelms = $id:RD.reader_module$.read_vint s in $e$ >> in

          let match_cases =
            List.filter_map maybe_match_case
              [
                "Vint", (fun e -> e), constant_match_cases;
                "Tuple", wrap_non_constant, nonconstant_match_cases;
              ]
          in

            <:expr< let t = $id:RD.reader_module$.read_prefix s in
              match Extprot.Codec.ll_type t with [
                $Ast.mcOr_of_list match_cases$
                | ll_type -> Extprot.Error.bad_wire_type ~ll_type ()
              ]
            >>
      | Message name ->
          <:expr< $uid:String.capitalize name$.$lid:RD.reader_func name$ s >>
      | Htuple (kind, llty) ->
          let e = match kind with
              List ->
                <:expr<
                  let rec loop acc = fun [
                      0 -> List.rev acc
                    | n -> let v = $read llty$ in loop [v :: acc] (n - 1)
                  ] in loop [] nelms
                >>
              | Array ->
                  <:expr<
                    match nelms with [
                        0 -> [||]
                      | n ->
                          let elm = $read llty$ in
                          let a = Array.make nelms elm in begin
                            for i = 1 to nelms - 1 do
                              a.(i) := $read llty$
                            done;
                            a
                          end
                    ]
                  >>
          in <:expr<
                let t = $id:RD.reader_module$.read_prefix s in
                  match Extprot.Codec.ll_type t with [
                      Extprot.Codec.Htuple ->
                        let len = $id:RD.reader_module$.read_vint s in
                        let nelms = $id:RD.reader_module$.read_vint s in
                          $e$
                    | ty -> Extprot.Error.bad_wire_type ~ll_type:ty ()
                  ]
              >>
    in read llty

  let record_case msgname ?constr tag fields =
    let _loc = Loc.mk "<generated code @ record_case>" in
    let constr_name = Option.default "<default>" constr in

    let read_field fieldno (name, mutabl, llty) ?default expr =
      let rescue_match_case = match default with
          None ->
            <:match_case<
              Extprot.Error.Extprot_error (e, loc) ->
                Extprot.Error.failwith_location
                  ~message:$str:msgname$
                  ~constructor:$str:constr_name$
                  ~field:$str:name$
                  e loc
            >>
        | Some expr ->
            <:match_case<
                Extprot.Error.Extprot_error
                  (Extprot.Error.Bad_format ((Extprot.Error.Bad_wire_type _) as e), loc) ->
                    Extprot.Error.failwith_location
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      e loc
              | Extprot.Error.Extprot_error _ -> $expr$ >> in
      let default_value = match default with
          Some expr -> expr
        | None ->
            <:expr< Extprot.Error.missing_field
                      ~message:$str:msgname$
                      ~constructor:$str:constr_name$
                      ~field:$str:name$
                      ()
            >>
      in
        <:expr<
           let $lid:name$ =
             if nelms >= $int:string_of_int (fieldno + 1)$ then
               try
                 $read_field msgname constr_name name llty$
               with [$rescue_match_case$]
             else
                 $default_value$
           in $expr$
        >> in

    let field_assigns =
      List.map
        (fun (name, _, _) -> <:rec_binding< $lid:name$ = $lid:name$ >>)
        fields in
    (* might need to prefix it with the constructor:  A { x = 1; y = 0 } *)
    let record =
      let r = <:expr< { $Ast.rbSem_of_list field_assigns$ } >> in match constr with
          None -> r
        | Some c -> <:expr< $uid:String.capitalize c$ $r$ >> in
    let e =
      List.fold_right
        (fun (i, fieldinfo) e -> read_field i fieldinfo e)
        (list_mapi (fun i x -> (i, x)) fields)
        record
    in
      <:match_case<
        $int:string_of_int tag$ ->
          let len = $id:RD.reader_module$.read_vint s in
          let nelms = $id:RD.reader_module$.read_vint s in
            $e$
            >>

  let rec read_message msgname =
    let _loc = Loc.mk "<generated code @ read_message>" in
    let wrap match_cases =
      <:expr<
        let t = $id:RD.reader_module$.read_prefix s in begin
          if Extprot.Codec.ll_type t <> Extprot.Codec.Tuple then
            Extprot.Error.bad_wire_type
              ~message:$str:msgname$ ~ll_type:(Extprot.Codec.ll_type t) ()
            else ();
          match Extprot.Codec.ll_tag t with [
            $match_cases$
            | tag -> Extprot.Error.unknown_tag ~message:$str:msgname$ tag
          ]
        end
      >>
    in
      function
        Record_single fields -> wrap (record_case msgname 0 fields)
      | Record_sum l ->
          list_mapi (fun tag (constr, fields) -> record_case msgname ~constr tag fields) l |>
            Ast.mcOr_of_list |> wrap
end

let add_message_reader bindings msgname mexpr c =
  let _loc = Loc.mk "<generated code @ add_message_reader>" in
  let llrec = Gencode.low_level_msg_def bindings mexpr in
  let module Mk_normal_reader =
    Make_reader(struct
                  let reader_module = <:ident< Extprot.Reader.String_reader >>
                  let reader_func = ((^) "read_")
                end) in
  let read_expr = Mk_normal_reader.read_message msgname llrec in
    {
      c with c_reader =
         Some <:str_item< value $lid:"read_" ^ msgname$ s = $read_expr$ >>
    }

let add_message_io_reader bindings msgname mexpr c =
  let _loc = Loc.mk "<generated code @ add_message_io_reader>" in
  let llrec = Gencode.low_level_msg_def bindings mexpr in
  let module Mk_io_reader =
    Make_reader(struct
                  let reader_module = <:ident< Extprot.Reader.IO_reader >>
                  let reader_func = ((^) "io_read_")
                end) in
  let ioread_expr = Mk_io_reader.read_message msgname llrec in
    {
      c with c_io_reader =
        Some <:str_item< value $lid:"io_read_" ^ msgname$ s = $ioread_expr$ >>
    }

let vint_length = function
    n when n < 128 -> 1
  | n when n < 13384 -> 2
  | n when n < 2097152 -> 3
  | n when n < 268435456 -> 4
  | _ -> 5 (* FIXME: checking for 64-bit and 32-bit archs *)

let rec write_field fname =
  let _loc = Loc.mk "<generated code @ write>" in
  let simple_write_func = function
      Vint Bool -> "write_bool"
    | Vint Int -> "write_relative_int"
    | Vint Positive_int -> "write_positive_int"
    | Bitstring32 -> "write_int32"
    | Bitstring64 Long -> "write_int64"
    | Bitstring64 Float -> "write_float"
    | Bytes -> "write_string"
    | Tuple _ | Sum _ | Htuple _ | Message _ -> assert false in

  let rec write_values tag var_tys =
    let nelms = List.length var_tys in
    let write_elms =
      List.map (fun (v, ty) -> write <:expr< $lid:v$ >> ty) var_tys
    in
      <:expr<
        let abuf =
          let aux = Extprot.Msg_buffer.create () in do {
            $Ast.exSem_of_list write_elms$;
            aux
          }
        in do {
          Extprot.Msg_buffer.add_tuple_prefix aux $int:string_of_int tag$;
          Extprot.Msg_buffer.add_vint aux
            (Extprot.Msg_buffer.length abuf +
             $int:string_of_int @@ vint_length nelms$);
          Extprot.Msg_buffer.add_vint aux $int:string_of_int nelms$;
          Extprot.Msg_buffer.add_buffer aux abuf
        }
      >>

  and write_tuple tag v lltys =
    let var_tys = list_mapi (fun i ty -> (sprintf "v%d" i, ty)) lltys in
    let patt =
      Ast.paCom_of_list @@ List.map (fun (v, _) -> <:patt< $lid:v$ >>) var_tys
    in
      <:expr<
        let $patt$ = $v$ in
          $write_values tag var_tys$
      >>

  and write v = function
      Vint _ | Bitstring32 | Bitstring64 _ | Bytes as llty ->
          <:expr< Extprot.Msg_buffer.$lid:simple_write_func llty$ aux $v$ >>
    | Message name ->
        <:expr< $uid:String.capitalize name$.$lid:"write_" ^ name$ aux $v$ >>
    | Tuple lltys -> write_tuple 0 v lltys
    | Htuple (kind, llty) ->
        let iter_f = match kind with
            Array -> <:expr< Array.iter >>
          | List -> <:expr< List.iter >>
        in
          <:expr<
            let write_elm aux v = $write <:expr< v >> llty$ in
            let nelms = ref 0 in
            let abuf = Extprot.Msg_buffer.create () in do {
                $iter_f$ (fun v -> do { write_elm abuf v; incr nelms } ) $v$;
                Extprot.Msg_buffer.add_htuple_prefix aux 0;
                Extprot.Msg_buffer.add_vint aux
                  (Extprot.Msg_buffer.length abuf +
                   Extprot.Codec.vint_length nelms.contents);
                Extprot.Msg_buffer.add_vint aux nelms.contents;
                Extprot.Msg_buffer.add_buffer aux abuf
              }
         >>
    | Sum (constant, non_constant) ->
        let constant_match_cases =
          List.map
            (fun c ->
               <:match_case<
                 $uid:String.capitalize c.const_type$.$lid:c.const_name$ ->
                   Extprot.Msg_buffer.add_const_prefix aux $int:string_of_int c.const_tag$
               >>)
            constant in
        let non_constant_cases =
          List.map
            (fun (c, lltys) ->
               let var_tys = list_mapi (fun i ty -> (sprintf "v%d" i, ty)) lltys in
               let patt =
                 List.map (fun (v, _) -> <:patt< $lid:v$ >>) var_tys |>
                 Ast.paCom_of_list
               in
                 <:match_case<
                   $uid:String.capitalize c.const_type$.$uid:c.const_name$
                     $patt$ -> $write_values c.const_tag var_tys$
                 >>)
            non_constant in
        let match_cases = constant_match_cases @ non_constant_cases in
          <:expr< match $v$ with [ $Ast.mcOr_of_list match_cases$ ] >>

  in write <:expr< msg.$lid:fname$ >>

let write_fields fs =
  Ast.exSem_of_list @@ List.map (fun (name, _, llty) -> write_field name llty) fs

let rec write_message msgname =
  let _loc = Loc.mk "<generated code @ write_message>" in
  let dump_fields tag fields =
    let nelms = List.length fields in
      <:expr<
         let aux = Extprot.Msg_buffer.create () in
         let nelms = $int:string_of_int nelms$ in do {
           Extprot.Msg_buffer.add_tuple_prefix b  $int:string_of_int tag$;
           $write_fields fields$;
           Extprot.Msg_buffer.add_vint b
             (Extprot.Msg_buffer.length aux +
              $int:string_of_int @@ vint_length nelms$);
           Extprot.Msg_buffer.add_vint b $int:string_of_int nelms$;
           Extprot.Msg_buffer.add_buffer b aux
         }
      >>

  in function
      Record_single fields -> dump_fields 0 fields
    | Record_sum l ->
        let match_case (tag, constr, fields) =
          <:match_case< $uid:constr$ msg -> $dump_fields tag fields$ >> in
        let match_cases =
          Ast.mcOr_of_list @@ List.map match_case @@
          List.mapi (fun i (c, fs) -> (i, c, fs)) l
        in <:expr< match msg with [ $match_cases$ ] >>

let add_message_writer bindings msgname mexpr c =
  let _loc = Loc.mk "<generated code @ add_message_writer>" in
  let llrec = Gencode.low_level_msg_def bindings mexpr in
  let write_expr = write_message msgname llrec in
  let writer = <:str_item< value $lid:"write_" ^ msgname$ b msg = $write_expr$ >> in
    { c with c_writer = Some writer }

let msgdecl_generators =
  [
    "reader", add_message_reader;
    "io_reader", add_message_io_reader;
    "writer", add_message_writer;
    "pretty_printer", Pretty_print.add_msgdecl_pretty_printer;
  ]

let typedecl_generators =
  [
    "pretty_printer", Pretty_print.add_typedecl_pretty_printer;
  ]
