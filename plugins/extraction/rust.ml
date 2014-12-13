open Miniml
open Pp
open Names

let pp_sig : ml_signature -> Pp.std_ppcmds = function
  | _ -> Pp.mt () (* TODO: should be improved *)

let pp_decl : ml_decl -> Pp.std_ppcmds = function
  | _ -> Pp.mt () (* TODO: should be improved *)

let pp_struct : ml_structure -> std_ppcmds = function
  | _ -> failwith "pp_struct not implemented"

let preamble : Id.t -> std_ppcmds option -> module_path list -> unsafe_needs ->
               std_ppcmds = function
  | _ -> failwith "preamble not implemented"

let keywords : Id.Set.t = Id.Set.empty (* TODO: some has to be added *)

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
