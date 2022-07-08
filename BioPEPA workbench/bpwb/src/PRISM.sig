(* 
 File: PRISM.sig

 PRISM compilation for the Bio-PEPA Workbench
*)

signature PRISM =
sig

  val PRISMid : string -> string
  val generate : (string * BioPEPA.Component * (string * string) list) -> unit

end;
