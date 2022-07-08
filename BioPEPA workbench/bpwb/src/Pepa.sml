(* 
   File: pepa.sml

   This is the root file for the MLj compilation process and 
   initiates Dizzy compilation.

   This version of the PEPA compiler compiles with MLj 0.1,
   but not 0.2
*)

structure pepa = struct 

_public _classtype T 
{

  _public _static _final _method "compile" (fileName : Java.String option) : unit =
      case fileName of 
	NONE => 
	   Error.fatal_error "File name supplied for compilation was null"
      | SOME fN => 
	   PWB.command (" -bridge " ^ (Java.toString fN))
(*

  _public _static _final _method "main" (env : Java.String option Java.array option) =
      case env of 
	NONE => 
	   PEPA2PRISM.main []
      | SOME env' => 
	let
	  val array = Java.toArray env'
	in
	  if Array.length array = 0 
	  then PEPA2PRISM.main []
	  else
	    case Array.sub(array, 0) of
	      NONE => PEPA2PRISM.main []
	    | SOME jstr =>
	      PEPA2PRISM.main [Java.toString jstr]
	end
*)

}


end;
