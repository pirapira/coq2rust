(* Yoichi Hirai, 2015.
   containing copies from scheme.ml, haskell.ml, and ocaml.ml
*)

open Miniml
open Util
open Pp
open Names
open Nameops
open Table
open Common
open Globnames
open Mlutil

let keywords : Id.Set.t = Id.Set.empty (* TODO: some has to be added *)

let pp_comment s = str "// " ++ s ++ fnl ()

let pp_sig : ml_signature -> Pp.std_ppcmds = function
  | _ -> mt () (* TODO: should be improved *)

let pp_cons c =
  let typ = str (pp_global Type (IndRef (inductive_of_constructor c))) in
  typ ++ str "::" ++ str (Common.pp_global Cons (ConstructRef c))

let pp_global k (r : global_reference) (cons_with_type : bool) =
  match k, r, cons_with_type with
    | Cons, ConstructRef c, true -> pp_cons c
    | _ -> str (Common.pp_global k r)

(*s Pretty-printing of types. [par] is a boolean indicating whether parentheses
    are needed or not. *)

let rec pp_type par vl t =
  let rec pp_rec par = function
    | Tmeta _ | Tvar' _ -> assert false
    | Tvar i -> str "a" ++ int i
    | Tglob (r,[]) -> pp_global Type r false
    | Tglob (IndRef(kn,0),l)
	when not (keep_singleton ()) && MutInd.equal kn (mk_ind "Coq.Init.Specif" "sig") ->
	  pp_type true vl (List.hd l)
    | Tglob (r,l) ->
	  pp_par par
	    (pp_global Type r false ++ spc () ++
	     prlist_with_sep spc (pp_type true vl) l)
    | Tarr (t1,t2) ->
	pp_par par
	  (pp_rec true t1 ++ spc () ++ str "->" ++ spc () ++ pp_rec false t2)
    | Tdummy _ -> str "()"
    | Tunknown -> str "()"
    | Taxiom -> str "() -- AXIOM TO BE REALIZED\n"
 in
  hov 0 (pp_rec par t)

let pr_typed_id (id, typ) = str (Id.to_string id) ++ str ": " ++ pp_type false [] typ

let pp_box_type par vl t =
  str "Box<" ++ (pp_type par vl t) ++ str ">"

let pp_one_ind ip pl cv =
  let pl : Id.t list = rename_tvars keywords pl in
  let pp_constructor (r,l) =
    (pp_global Cons r false ++
     match l with
       | [] -> (mt ())
       | _  -> (str "(" ++
      	       	prlist_with_sep
		  (fun () -> (str ", ")) (pp_box_type true pl) l)
	        ++ str ")"
               )
  in
  str "enum " ++
  pp_global Type (IndRef ip) false ++
  (prlist_strict (fun id -> str " " ++ (Nameops.pr_id id)) pl) ++ str " {" ++
    fnl () ++ str " " ++
       v 0 (  prvect_with_sep (fun () -> str "," ++ fnl()) pp_constructor
		(Array.mapi (fun i c -> ConstructRef (ip,i+1),c) cv))
    ++ fnl() ++ str "}" ++ fnl()

let pp_logical_ind packet =
  pp_comment (Nameops.pr_id packet.ip_typename ++ str " : logical inductive") ++
  pp_comment (str "with constructors : " ++
	      prvect_with_sep spc Nameops.pr_id packet.ip_consnames)

let pp_singleton kn packet =
  let l = rename_tvars keywords packet.ip_vars in
  let l' = List.rev l in
  hov 2 (str "type " ++ pp_global Type (IndRef (kn,0)) false ++ spc () ++
	 prlist_with_sep spc pr_id l ++
	 (if not (List.is_empty l) then str " " else mt ()) ++ str "=" ++ spc () ++
	 pp_type false l' (List.hd packet.ip_types.(0)) ++ str ";" ++ fnl () ++
	 pp_comment (str "singleton inductive, whose constructor was " ++
		     pr_id packet.ip_consnames.(0)))

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

let pr_binding (lst : (Id.t * ml_type) list) : std_ppcmds =
  match lst with
    | [] -> str "()"
    | l -> pp_par true (prlist_with_sep (fun () -> str ", ") pr_typed_id l)

let expr_needs_par = function
  | MLlam _  -> true
  | MLcase (_,_,[|_|]) -> false
  | MLcase (_,_,pv) -> true
  | _        -> false


(*s [collect_lams MLlam(id1,...MLlam(idn,t)...)] returns
    [[idn;...;id1]] and the term [t]. *)

let collect_lams =
  let rec collect acc = function
    | (MLlam(id,t), Tarr (a, b)) -> collect ((id, a)::acc) (t,b)
    | (x,y)           -> acc,x,y
  in collect []

let rec rename_vars avoid = function
  | [] ->
      [], avoid
  | (id, t) :: idl when id == dummy_name ->
      (* we don't rename dummy binders *)
      let (idl', avoid') = rename_vars avoid idl in
      ((id, t) :: idl', avoid')
  | (id, t) :: idl ->
      let (idl, avoid) = rename_vars avoid idl in
      let id = rename_id (lowercase_id id) avoid in
      ((id, t) :: idl, Id.Set.add id avoid)

let push_vars ids ((db,avoid) : Common.env) =
  let ids',avoid' = rename_vars avoid ids in
  ids', ((List.map fst ids') @ db, avoid')

let pp_apply st par args = match args with
  | [] -> st
  | _  -> hov 2 (pp_par par (st ++ spc () ++ pp_par true (prlist_with_sep (fun () -> str ",") identity args)))

let pp_apply2 st par args =
  let par' = not (List.is_empty args) || par in
  pp_apply (pp_par par' st) par args

let rec pp_expr par env args =
  let apply st = pp_apply st par args
  and apply2 st = pp_apply2 st par args in
  function
    | MLrel n ->
      let id = get_db_name n env in apply (pr_id id)
    | MLapp (f,args') ->
	let stl = List.map (pp_expr true env []) args' in
        pp_expr par env (stl @ args) f
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
	apply (pp_global Term r false)
    | MLcons (typ,r,a) as c ->
        assert (List.is_empty args);
        begin match a with
	  | _ when is_native_char c ->
	    let _ = failwith "native_char not implemented" in
	    pp_native_char c
	  | [] ->
	    pp_global Cons r true
	  | [a] ->
	    pp_par par (pp_global Cons r true
		         ++ pp_par true (pp_box_expr true env [] a))
	  | _ ->
	    pp_par par (pp_global Cons r true ++
                        pp_par true (prlist_with_sep (fun _ -> str ", ")
                                     (pp_box_expr true env []) a))
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
        apply2
	  (v 0 (str "match " ++ pp_expr false env [] t ++ str " {" ++
		fnl () ++ pp_pat env pv ++ fnl() ++ str "}"))
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
and pp_cons_pat par r ppl =
  let ppl = List.map (fun pp -> str "box " ++ pp ) ppl in
  pp_par par
    (pp_global Cons r true ++
       if List.is_empty ppl then mt() else
	 pp_par true (prlist_with_sep (fun () -> str ",") identity ppl))

and pp_gen_pat ids env = function
  | Pcons (r, l) -> failwith "pp_gen_pat0" (* pp_cons_pat r (List.map (pp_gen_pat ids env) l) *)
  | Pusual r -> pp_cons_pat false r (List.map pr_id ids)
  | Ptuple l -> failwith "pp_gen_pat2" (* pp_boxed_tuple (pp_gen_pat ids env) l*)
  | Pwild -> str "_"
  | Prel n -> pr_id (get_db_name n env)

and pp_one_pat env (ids,p,t) =
  let ids',env' = Common.push_vars (List.rev_map id_of_mlid ids) env in
  pp_gen_pat (List.rev ids') env' p,
  pp_expr (expr_needs_par t) env' [] t

and pp_pat env pv =
  prvecti
    (fun i x ->
       let s1,s2 = pp_one_pat env x in
       hv 2 (hov 4 (s1 ++ str " =>") ++ spc () ++ hov 2 s2) ++
       if Int.equal i (Array.length pv - 1) then mt () else str "," ++ fnl ())
    pv

and pp_function env f t typ =
  let bl,t',typ = collect_lams (t, typ) (* collect_lambs should work on type as well *) in
  let bl = List.map (fun i -> (id_of_mlid (fst i), snd i)) bl in
  let bl,env' = push_vars bl env in
  match t' with
    | MLcase(Tglob(r,_),MLrel 1,pv) when
	not (is_coinductive r) && List.is_empty (get_record_fields r) &&
	not (is_custom_match pv) ->
(*	if not (ast_occurs 1 (MLcase(Tunknown,MLdummy,pv))) then
	  pr_binding (List.rev (List.tl bl)) ++
       	  str " = function" ++ fnl () ++
	  v 0 (pp_pat env' pv)
	else *)
          pr_binding (List.rev bl) ++ str " -> " ++ pp_type false [] typ ++
          str " { match " ++ pr_id (fst (List.hd bl)) ++ str " {" ++ fnl () ++
	  v 0 (pp_pat env' pv)
	  ++ fnl() ++ str "} }"
    | _ ->
     (pr_binding (List.rev bl)) ++ str " -> " ++ pp_type false [] typ ++
	  str " {" ++ fnl () ++ str "  " ++
	  hov 2 (pp_expr false env' [] t') ++ fnl() ++ str "}"
and pp_box_expr par env args term =
  pp_par par ((str "box () ") ++ pp_expr true env args term)

let pp_fn e typ =
  hov 4 (str "// " ++ e ++ str " :" ++ spc () ++ pp_type false [] typ)  ++ fnl2 ()

let pp_decl : ml_decl -> Pp.std_ppcmds = function
  | Dind (kn, ind) when ind.ind_kind = Singleton ->
      pp_singleton kn ind.ind_packets.(0) ++ fnl ()
  | Dind (kn, ind) ->
    hov 0 (pp_ind kn 0 ind)
  | Dtype (_, _, _) -> failwith "Dtype not implemented"
  | Dterm (r, a, t) ->
    if Table.is_inline_custom r then failwith "inline custom term not implemented"
    else
      let e = pp_global Term r in
	if is_custom r then
	  failwith "custom term printing not implemented"
	  (* hov 0 (e ++ str " = " ++ str (find_custom r) ++ fnl2 ()) *)
	else
	  let name = pp_global Term r false in
	  pp_fn name t ++
	  hov 0 (str "fn " ++ name ++ pp_function (empty_env ()) e a t ++ fnl2 ())
  | Dfix (rv, defs, typs) ->
      let names = Array.map
	(fun r -> if is_inline_custom r then mt () else pp_global Term r false) rv
      in
      prvecti
	(fun i r ->
	  let void = is_inline_custom r ||
	    (not (is_custom r) && match defs.(i) with MLexn "UNUSED" -> true | _ -> false)
	  in
	  if void then mt ()
	  else
	    (if is_custom r then
		(names.(i) ++ str " = " ++ str (find_custom r))
	     else
		str "fn " ++ names.(i) ++ (pp_function (empty_env ()) names.(i) defs.(i)) (typs.(i)))
	    ++ fnl2 ())
	rv

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
