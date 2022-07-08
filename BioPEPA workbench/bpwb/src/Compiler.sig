(* 
 File: Compiler.sig

 The compiler from PEPA nets to PEPA
*)

signature Compiler =
sig

  (* Compile a PEPA net component to a list of PEPA components *)
  val compile : PepaNets.Component -> PepaNets.Component list

  (* Compile a PEPA net component from a .pepa file to a .pwb file *)
  val compileFromFile : string -> unit

end;
