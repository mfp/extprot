
open Ptypes
open Gencode
open Camlp4
open PreCast
open Ast

type container = {
  c_name : string;
  c_types : Ast.str_item option;
  c_code : Ast.str_item option;
}

let (|>) x f = f x


let generate_container bindings =
  let _loc = Loc.mk "gen_OCaml" in

  let typedecl name ?(params = []) ctyp =
    Ast.TyDcl (_loc, name, params, ctyp, []) in

  let typedef name ?(params = []) ctyp =
      <:str_item< type $typedecl name ~params ctyp $ >> in

  let rec message_types msgname = function
      `Record l ->
      (* let fst_lbl, rest = match l with *)
          (* hd::tl -> hd, tl *)
        (* | _ -> failwith "generate_container: expected at least one field" in *)
      let fields =
        List.fold_left
          (fun ctyp (name, mutabl, ty) ->
             let tyexpr = ctyp_of_texpr ty in
               if mutabl then
                 <:ctyp< $ctyp$; $lid:name$ : mutable $tyexpr$ >>
               else
                 <:ctyp< $ctyp$; $lid:name$ : $tyexpr$ >>)
          <:ctyp< >>
          l
      (* no quotations for type, wtf? *)
      (* in <:str_item< type $msgname$ = { $fields$ } >> *)
      in typedef msgname <:ctyp< { $fields$ } >>

   | `Sum l ->
       let record_types =
         List.fold_left
           (fun str_item (const, mexpr) ->
              let tydef = message_types (msgname ^ "_" ^ const) (mexpr :> message_expr) in
                <:str_item< $str_item$; $tydef$ >>)
           <:str_item< >>
           l in
       let consts =
         List.fold_left
           (fun cs (const, _) -> <:ctyp< $cs$ | $uid:const$ of ($lid:msgname ^ "_" ^ const$) >>)
           <:ctyp< >>
           l
       in <:str_item< $record_types$; $typedef msgname <:ctyp< [$consts$] >>$ >>

  and ctyp_of_texpr expr =
    type_expr expr |> reduce_to_poly_texpr_core bindings |> ctyp_of_poly_texpr_core

  and ctyp_of_poly_texpr_core = function
      `Bool -> <:ctyp< bool >>
    | `Byte -> <:ctyp< char >>
    | `Int _ -> <:ctyp< int >>
    | `Long_int -> <:ctyp< Int64.t >>
    | `Float -> <:ctyp< float >>
    | `String -> <:ctyp< string >>
    | `List ty -> <:ctyp< list $ctyp_of_poly_texpr_core ty$ >>
    | `Array ty -> <:ctyp< array $ctyp_of_poly_texpr_core ty$ >>
    | `Tuple l ->
        List.fold_right
          (fun ptexpr tup -> <:ctyp< ( $ ctyp_of_poly_texpr_core ptexpr $ * $tup$ ) >>)
          l <:ctyp< >>
    | `Type (name, args) ->
        let t = List.fold_left (* apply *)
                  (fun ty ptexpr -> <:ctyp< $ty$ $ctyp_of_poly_texpr_core ptexpr$ >>)
                  <:ctyp< >>
                  args
        in (try <:ctyp< $id:Ast.ident_of_ctyp t$ >> with Invalid_argument _ -> t)
    | `Type_arg n -> <:ctyp< '$n$ >>

  in function
      Message_decl (msgname, mexpr) ->
        Some {
          c_name = msgname;
          c_types = Some (message_types msgname mexpr);
          c_code = None;
        }
    | Type_decl (name, params, texpr) ->
        (* declare type params as bogus sum types, so they are not expanded in
         * the beta reduction phase *)
        let bogus_sum name =
          {
            type_name = name;
            constant = [];
            non_constant = []
          } in
        let bogus_decls =
          List.map
            (fun name -> (name, Type_decl ("<bogus>", [], `Sum (bogus_sum name))))
            params in
        let bindings =
          update_bindings bindings
            (List.map fst bogus_decls) (List.map snd bogus_decls) in
        let ty = match poly_beta_reduce_texpr bindings texpr with
            `Sum s ->
              let const = List.fold_left
                           (fun ctyp ty -> <:ctyp< $ctyp$ | $uid:ty$ >>)
                           <:ctyp< >> s.constant

              in List.fold_left
                   (fun ctyp (const, ptexprs) ->
                      let ty = ctyp_of_poly_texpr_core (`Tuple ptexprs) in
                        <:ctyp< $ctyp$ | $uid:const$ of $ty$ >>)
                   const
                   s.non_constant
          | #poly_type_expr_core ->
              reduce_to_poly_texpr_core bindings texpr |> ctyp_of_poly_texpr_core in
        let params = List.map (fun n -> <:ctyp< '$lid:n$ >>) params in
          Some {
            c_name = name;
            c_types = Some <:str_item< type $typedecl name ~params ty$ >>;
            c_code = None;
          }
