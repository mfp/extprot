
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

let foldl1 msg f g = function
    [] -> invalid_arg ("foldl1: empty list -- " ^ msg)
  | hd::tl -> List.fold_left g (f hd) tl

let foldr1 msg f g = function
    [] -> invalid_arg ("foldr1: empty list -- " ^ msg)
  | l -> match List.rev l with
        [] -> assert false
      | hd::tl -> List.fold_left (fun s x -> g x s) (f hd) tl

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
    | `Byte -> <:ctyp< char >>
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
                  <:ctyp< $lid:name$ >>
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
            (fun name -> (name, Type_decl ("bogus", [], `Sum (bogus_sum name))))
            params in
        let bindings =
          update_bindings bindings
            (List.map fst bogus_decls) (List.map snd bogus_decls) in
        let ty = match poly_beta_reduce_texpr bindings texpr with
            `Sum s -> begin
              let ty_of_const_texprs (const, ptexprs) =
                let ty = ctyp_of_poly_texpr_core (`Tuple ptexprs) in
                  <:ctyp< $uid:const$ of $ty$ >>

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
        let params = List.map (fun n -> <:ctyp< '$lid:n$ >>) params in
          Some {
            c_name = name;
            c_types = Some <:str_item< type $typedecl name ~params ty$ >>;
            c_code = None;
          }
