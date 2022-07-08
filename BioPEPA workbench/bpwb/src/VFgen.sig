(* 
 File: VFgen.sig

 VFgen compilation for the Bio-PEPA Workbench
*)

signature VFgen =
sig

  val VFgenId : string -> string
  val generate : (string * BioPEPA.Component * (string * string) list) -> unit

end;
