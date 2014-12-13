(* Yoichi Hirai, 2015.
   containing copies from scheme.ml, haskell.ml
*)

open Miniml
open Util
open Pp
open Names
open Table
open Common
open Globnames
open Mlutil

let keywords : Id.Set.t = Id.Set.empty (* TODO: some has to be added *)

let pp_comment s = str "// " ++ s ++ fnl ()

let pp_sig : ml_signature -> Pp.std_ppcmds = function
  | _ -> mt () (* TODO: should be improved *)

(*s Pretty-printing of types. [par] is a boolean indicating whether parentheses
    are needed or not. *)

let rec pp_type par vl t =
  let rec pp_rec par = function
    | Tmeta _ | Tvar' _ -> assert false
    | Tvar i -> str "a" ++ int i
    | Tglob (r,[]) -> str (pp_global Type r)
    | Tglob (IndRef(kn,0),l)
	when not (keep_singleton ()) && MutInd.equal kn (mk_ind "Coq.Init.Specif" "sig") ->
	  pp_type true vl (List.hd l)
    | Tglob (r,l) ->
	  pp_par par
	    (str (pp_global Type r) ++ spc () ++
	     prlist_with_sep spc (pp_type true vl) l)
    | Tarr (t1,t2) ->
	pp_par par
	  (pp_rec true t1 ++ spc () ++ str "->" ++ spc () ++ pp_rec false t2)
    | Tdummy _ -> str "()"
    | Tunknown -> str "()"
    | Taxiom -> str "() -- AXIOM TO BE REALIZED\n"
 in
  hov 0 (pp_rec par t)

let pp_one_ind ip pl cv =
  let pl : Id.t list = rename_tvars keywords pl in
  let pp_constructor (r,l) =
    (str (pp_global Cons r) ++
     match l with
       | [] -> (mt ())
       | _  -> (str " " ++
      	       	prlist_with_sep
		  (fun () -> (str " ")) (pp_type true pl) l))
  in
  str (if Array.is_empty cv then "type " else "data ") ++
  str (pp_global Type (IndRef ip)) ++
  (prlist_strict (fun id -> str " " ++ (Nameops.pr_id id)) pl) ++ str " =" ++
  if Array.is_empty cv then str " () -- empty inductive"
  else
    (fnl () ++ str " " ++
     v 0 (str "  " ++
	  prvect_with_sep (fun () -> fnl () ++ str "| ") pp_constructor
	    (Array.mapi (fun i c -> ConstructRef (ip,i+1),c) cv)))

let pp_logical_ind packet =
  pp_comment (Nameops.pr_id packet.ip_typename ++ str " : logical inductive") ++
  pp_comment (str "with constructors : " ++
	      prvect_with_sep spc Nameops.pr_id packet.ip_consnames)

let rec pp_ind kn i ind =
  if i >= Array.length ind.ind_packets then
    fnl ()
  else
    let ip = (kn,i) in
    let p = ind.ind_packets.(i) in
    if is_custom (IndRef (kn,i)) then pp_ind kn (i+1) ind
    else
      if p.ip_logical then
	pp_logical_ind p ++ pp_ind kn (i+1) ind
      else
	pp_one_ind ip p.ip_vars p.ip_types ++ fnl () ++
	pp_ind kn (i+1) ind

let pp_decl : ml_decl -> Pp.std_ppcmds = function
  | Dind (kn, ind) when ind.ind_kind = Singleton -> failwith "singleton not implemented"
  | Dind (kn, ind) ->
    hov 0 (pp_ind kn 0 ind)
  | Dtype (_, _, _) -> failwith "Dtype not implemented"
  | Dterm (r, a, _) ->
    if Table.is_inline_custom r then failwith "inline custom term not implemented"
    else
      str "term"
  | Dfix (_, _, _) -> failwith "Dfix not implemented"

let rec pp_structure_elem = function
  | (l, SEdecl d) -> pp_decl d
  | (l, SEmodule m) -> failwith "semodule not implemented"
  | (l, SEmodtype m) -> failwith "SEmodtype not implemented"

let pp_struct (elms : ml_structure) : std_ppcmds =
  let pp_sel (mp, sel) =
    push_visible mp [];
    let p = prlist_strict pp_structure_elem sel in
    pop_visible (); p
  in
  prlist_strict pp_sel elms

let preamble _ comment _ usf =
  str "// I am not sure but this place needs lots of characters\n" ++
  str "// Otherwise nothing is usually printed\n" ++
  str "//\n\n"

let rust_descr = {
  keywords = keywords;
  file_suffix = ".rs";
  preamble = preamble;
  pp_struct = pp_struct;
  sig_suffix = None;
  sig_preamble = (fun _ _ _ _ -> Pp.mt ());
  pp_sig = pp_sig;
  pp_decl = pp_decl
}
