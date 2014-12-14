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
      let _ = failwith "arrow has to be changed" in
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
  str (if Array.is_empty cv then "type " else "enum ") ++
  str (pp_global Type (IndRef ip)) ++
  (prlist_strict (fun id -> str " " ++ (Nameops.pr_id id)) pl) ++ str " {" ++
    begin
      if Array.is_empty cv then (let _ = failwith "how to print empty inductive" in str " () -- empty inductive")
      else
	(fnl () ++ str " " ++
	   v 0 (str "  " ++
		  prvect_with_sep (fun () -> str "," ++ fnl()) pp_constructor
		  (Array.mapi (fun i c -> ConstructRef (ip,i+1),c) cv)))
    end ++ fnl() ++
  str "}" ++ fnl()

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

let rec pp_expr par env args =
  let apply st = pp_apply st par args
  and apply2 st = pp_apply2 st par args in
  function
    | MLrel n ->
      failwith "MLrel not implemented"
      (* let id = get_db_name n env in apply (pr_id id) *)
    | MLapp (f,args') ->
      failwith "MLapp not implemented"
    (*
	let stl = List.map (pp_expr true env []) args' in
        pp_expr par env (stl @ args) f*)
    | MLlam _ as a ->
      failwith "MLlam not implemented"
	(*
	let fl,a' = collect_lams a in
	let fl,env' = push_vars (List.map id_of_mlid fl) env in
	let st = (pp_abst (List.rev fl) ++ pp_expr false env' [] a') in
	apply2 st *)
    | MLletin (id,a1,a2) ->
      failwith "MLletin not implemented"
        (* let i,env' = push_vars [id_of_mlid id] env in *)
	(* let pp_id = pr_id (List.hd i) *)
	(* and pp_a1 = pp_expr false env [] a1 *)
	(* and pp_a2 = pp_expr (not par && expr_needs_par a2) env' [] a2 in *)
	(* let pp_def = *)
	(*   str "let {" ++ cut () ++ *)
	(*   hov 1 (pp_id ++ str " = " ++ pp_a1 ++ str "}") *)
	(* in *)
	(* apply2 (hv 0 (hv 0 (hv 1 pp_def ++ spc () ++ str "in") ++ *)
	(* 	       spc () ++ hov 0 pp_a2)) *)
    | MLglob r ->
      failwith "MLglob not implemented"
	(* apply (pp_global Term r) *)
    | MLcons (typ,r,a) as c ->
        assert (List.is_empty args);
        begin match a with
	  | _ when is_native_char c ->
	    let _ = failwith "native_char not implemented" in
	    pp_native_char c
	  | [] ->
	    pp_type false [(* TODO: really empty? *)] typ
	     ++ str "::" ++ str (pp_global Cons r)
	  | [a] ->
	    let _ = failwith "singleton MLcons not implemented" in
	    pp_par par (str (pp_global Cons r) ++ spc () ++ pp_expr true env [] a)
	  | _ ->
	    let _ = failwith "other MLcons not implemented" in
	    pp_par par (str (pp_global Cons r) ++ spc () ++
			prlist_with_sep spc (pp_expr true env []) a)
	end
    | MLtuple l ->
      failwith "MLtuple not implemented"
        (* assert (List.is_empty args); *)
        (* pp_boxed_tuple (pp_expr true env []) l *)
    | MLcase (_,t, pv) when is_custom_match pv ->
      failwith "MLcase not implemented"
        (* if not (is_regular_match pv) then *)
	(*   error "Cannot mix yet user-given match and general patterns."; *)
	(* let mkfun (ids,_,e) = *)
	(*   if not (List.is_empty ids) then named_lams (List.rev ids) e *)
	(*   else dummy_lams (ast_lift 1 e) 1 *)
	(* in *)
	(* let pp_branch tr = pp_expr true env [] (mkfun tr) ++ fnl () in *)
	(* let inner = *)
	(*   str (find_custom_match pv) ++ fnl () ++ *)
	(*   prvect pp_branch pv ++ *)
	(*   pp_expr true env [] t *)
	(* in *)
	(* apply2 (hov 2 inner) *)
    | MLcase (typ,t,pv) ->
      failwith "MLcase not implemented"
        (* apply2 *)
	(*   (v 0 (str "case " ++ pp_expr false env [] t ++ str " of {" ++ *)
	(* 	fnl () ++ pp_pat env pv)) *)
    | MLfix (i,ids,defs) ->
      failwith "MLfix not implemented"
	(* let ids',env' = push_vars (List.rev (Array.to_list ids)) env in *)
	(* pp_fix par env' i (Array.of_list (List.rev ids'),defs) args *)
    | MLexn s ->
      failwith "MLexn not implemented"
	(* (\* An [MLexn] may be applied, but I don't really care. *\) *)
	(* pp_par par (str "Prelude.error" ++ spc () ++ qs s) *)
    | MLdummy ->
      failwith "MLdummy not implemented"
	(* str "__" (\* An [MLdummy] may be applied, but I don't really care. *\) *)
    | MLmagic a ->
      failwith "MLmagic not implemented"
	(* pp_apply (str "unsafeCoerce") par (pp_expr true env [] a :: args) *)
    | MLaxiom -> pp_par par (str "Prelude.error \"AXIOM TO BE REALIZED\"")
and pp_function env f t typ =
  let bl,t' = collect_lams t in
  let bl,env' = push_vars (List.map id_of_mlid bl) env in
  (str "fn " ++ f ++ pr_binding (List.rev bl) ++
     str "()" ++
     str "-> " ++ pp_type false [] typ ++
     str " {" ++ fnl () ++ str "  " ++
     hov 2 (pp_expr false env' [] t')) ++ fnl () ++ str "}"

let pp_decl : ml_decl -> Pp.std_ppcmds = function
  | Dind (kn, ind) when ind.ind_kind = Singleton -> failwith "singleton not implemented"
  | Dind (kn, ind) ->
    hov 0 (pp_ind kn 0 ind)
  | Dtype (_, _, _) -> failwith "Dtype not implemented"
  | Dterm (r, a, t) ->
    if Table.is_inline_custom r then failwith "inline custom term not implemented"
    else
      let e = str (pp_global Term r) in
	if is_custom r then
	  failwith "custom term printing not implemented"
	  (* hov 0 (e ++ str " = " ++ str (find_custom r) ++ fnl2 ()) *)
	else
	  hov 0 (pp_function (empty_env ()) e a t ++ fnl2 ())
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
